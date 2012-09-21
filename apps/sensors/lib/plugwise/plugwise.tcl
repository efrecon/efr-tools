package require uobj
package require event

namespace eval ::plugwise {
    variable PWISE
    if {![info exists PWISE] } {
	array set PWISE {
	    globals      "-controller"
	    -controller  "plugwise_util"
	    -frequency   5
	    -state       off
	    -dev         /dev/ttyUSB0
	    -react       10
	    -errors      4
	    dft_mac_pfx  000D6F0000
	    debug        off
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log plugwise PWISE
	::uobj::install_defaults plugwise PWISE
    }
}

# IMPLEMENTATION NOTES
#
# We rely on the (python-based) external binary that is able to
# control plugwise plugs via their serial interface.  Whenever a plug
# is created, an object is created and we initialise the plug from its
# current state (mainly to get the state of the relay).  Once done, we
# poll the state of the plug from time to time, both to gets its
# current power consumption and to get its state.
#
# Note that plugwise_util takes its arguments in sequence and executes
# them in sequence, which we use a number of times to minimise the
# number of forks.  For example, at initialisation time, we both
# synchronise the time of the plug to the computer time *and* get
# information from it in a single call to plugwise_util.
# 
# Ideally, we wanted to use the -c option of the plugwise_util, but
# since it does not flush, the pipe does not contain anything until
# the process has ended, so we cannot get continuous data.  Instead we
# have to poll, starting a new process every xx seconds, and this for
# each plug.  To spread the load, the initial check for status is
# delayed using a random number so as to minimise the load on the
# resources.


# TODO
#
# Better error handling when talking to plugs. What happens if
# plugwise_util fails?
#
# Handle plugs that are of wrong fw_version, since we know that
# plugwise_util is able get information from them but perhaps not able
# to control them?


# ::plugwise::mac -- Return a plugwise MAC address
#
#       Sanitise a MAC address to return a unified MAC address of 8
#       bytes, as used within the plugwise network.  The procedure
#       accepts short 3 bytes addresses and automatically adds the
#       prefix that seems to be prevalent to all installations.
#
# Arguments:
#	mac	MAC address, separated or not by : or - (3 or 8 bytes hex)
#
# Results:
#       Return a sanitised, uppercase, no separators MAC address coded
#       on exactly 8 bytes, or an empty string when sanitisation
#       failed.
#
# Side Effects:
#       None.
proc ::plugwise::mac { mac } {
    variable PWISE
    variable log

    set hex "\[A-Fa-f0-9\]\[A-Fa-f0-9\]"
    if { [string match \
	      ${hex}:${hex}:${hex}:${hex}:${hex}:${hex}:${hex}:${hex} $mac] } {
	foreach { a b c d e f g h } [split $mac ":"] break
	return [string toupper $a][string toupper $b][string toupper $c][string toupper $d][string toupper $e][string toupper $f][string toupper $g][string toupper $h]
    } elseif { [string match \
		    ${hex}-${hex}-${hex}-${hex}-${hex}-${hex}-${hex}-${hex} $mac] } {
	foreach { a b c d e f g h } [split $mac "-"] break
	return [string toupper $a][string toupper $b][string toupper $c][string toupper $d][string toupper $e][string toupper $f][string toupper $g][string toupper $h]
    } elseif { [string match \
		    ${hex}${hex}${hex}${hex}${hex}${hex}${hex}${hex} $mac] } {
	return [string toupper $mac]
    } elseif { [string match ${hex}:${hex}:${hex} $mac] } {
	foreach {a b c} [split $mac ":"] break
	return $PWISE(dft_mac_pfx)[string toupper $a][string toupper $b][string toupper $c]
    } elseif { [string match ${hex}-${hex}-${hex} $mac] } {
	foreach {a b c} [split $mac "-"] break
	return $PWISE(dft_mac_pfx)[string toupper $a][string toupper $b][string toupper $c]
    } elseif { [string match ${hex}${hex}${hex} $mac] } {
	return $PWISE(dft_mac_pfx)[string toupper $mac]
    }

    return "";  # Parsing error
}


proc ::plugwise::switch { p { state "" } } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    if { $state ne "" } {
	${log}::notice "Changing state of $PLUG(mac) to $state"
	# Make a command that starts by changing the state and then
	# reads the current state to check that we were successful.
	set cmd "|\"$PWISE(-controller)\" -m $PLUG(mac) -d $PLUG(-dev)\
                  -s [string is true $state] -q relay_state"
	set fd [open $cmd]
	fconfigure $fd -buffering line -blocking 1 -translation lf
	set state [string trim [read $fd]]
	close $fd
	set PLUG(-state) $state
	${log}::info "Plug $PLUG(mac) now in state: $state"
	::event::generate $p Switch
    }

    return $PLUG(-state)
}


proc ::plugwise::get { p what } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    ::switch -glob -- $what {
	mac -
	state {
	    return $PLUG($what)
	}
	hz -
	hw_ver -
	fw_ver {
	    return $PLUG(nfo:$what)
	}
	-* {
	    return [config $p $what]
	}
    }

    return ""
}


proc ::plugwise::destroy { p } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    ::event::generate $p Delete

    __pulse $p delete
    ::uobj::delete $p
}


proc ::plugwise::__pulse { p { op "next" } } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    ::switch $op {
	delete -
	destroy -
	clean {
	    if { $PLUG(poller) ne "" } {
		after cancel $PLUG(poller)
	    }
	    set PLUG(poller) ""
	}
	next {
	    if { $PLUG(poller) ne "" } {
		after cancel $PLUG(poller)
	    }
	    set when [expr {int($PLUG(-frequency)*1000)}]
	    set PLUG(poller) [after $when [namespace current]::__check $p]
	}
	first -
	init {
	    if { $PLUG(poller) ne "" } {
		after cancel $PLUG(poller)
	    }
	    set when [expr {int(rand()*$PLUG(-frequency)*1000)}]
	    set PLUG(poller) [after $when [namespace current]::__check $p]
	}
    }
    return $PLUG(poller)
}


proc ::plugwise::__check { p } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    if { $PLUG(state) eq "INITED" } {
	${log}::debug "Checking state of plug $PLUG(mac)"
	set cmd "|\"$PWISE(-controller)\" -m $PLUG(mac) -d $PLUG(-dev)\
                  -p -q relay_state"
    
	set fd [open $cmd]
	fconfigure $fd -buffering line -blocking 1 -translation lf

	set errs 0
	while { ! [eof $fd] } {
	    set l [string trim [string tolower [gets $fd]]]
	    if { $l ne "" } {
		if { [string is true $PWISE(debug)] } {
		    ${log}::debug "Status: $l"
		}
		if { [string match "power usage:*" $l] } {
		    foreach {- use} [split $l ":"] break
		    set use [string trim [string trim $use "w"]]
		    set old [lindex $PLUG(usage) end]
		    set now [clock seconds]
		    lappend PLUG(usage) $now $use
		    if { $old ne "" } {
			set diff [expr {$use-$old}]
			if { [expr {abs($diff)}] >= $PLUG(-react) } {
			    ::event::generate $p Demand [list %d $diff %t $now]
			}
		    }
		} elseif { [string match "error:*" $l] } {
		    ${log}::error "Error when communicating with plugwise: $l"
		    incr errs
		} else {
		    set old [string is true $PLUG(-state)]
		    set PLUG(-state) [string is true $l]
		    if { $PLUG(-state) != $old } {
			::event::generate $p Switch
		    }
		}
	    }
	}

	close $fd
    }

    # Put the plug in error state if we've failed getting data from it
    # for a number of times.
    if { $errs == 0 } {
	if { $PLUG(errors) > 0 } {
	    ${log}::debug "Plug $PLUG(mac) is fine again"
	}
	set PLUG(errors) 0
    } else {
	incr PLUG(errors) $errs
    }
    if { $PLUG(errors) > $PLUG(-errors) } {
	set PLUG(state) ERROR
    }

    if { $PLUG(state) eq "ERROR" } {
	::event::generate $p Error
	__pulse $p delete
    } else {
	__pulse $p next
    }
}


proc ::plugwise::jsdate { str } {
    variable PWISE
    variable log

    set str [string trim [string tolower $str]]
    if { [regexp {datetime.datetime\((.*)\)} $str - dt] } {
	foreach {Y m d H M S} [split $dt ","] break
	if { $S eq "" } { set S 00 }
	set S [string range 0[string trim $S] end-1 end]
	set M [string range 0[string trim $M] end-1 end]
	set H [string range 0[string trim $H] end-1 end]
	set d [string range 0[string trim $d] end-1 end]
	set m [string range 0[string trim $m] end-1 end]
	return [clock scan "$Y-$m-$d $H:$M:$S" -format "%Y-%m-%d %H:%M:%S"]
    }
    return [clock seconds]
}


proc ::plugwise::init { p } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    # Synchronise the clock with computer clock on initialisation and
    # get information about device
    set cmd "|\"$PWISE(-controller)\" -m $PLUG(mac) -d $PLUG(-dev) -t sync -i"
    
    ${log}::debug "Initialising plug $PLUG(mac)"
    set fd [open $cmd]
    fconfigure $fd -buffering line -blocking 1 -translation lf
    while { ! [eof $fd] } {
	set line [string trim [gets $fd]]
	if { [string is true $PWISE(debug)] } {
	    ${log}::debug "Initialisation: $line"
	}
	if { [string match -nocase info* $line] } {
	} elseif { $line eq "" } {
	} elseif { [string match -nocase "Error:*" $line] } {
	    ${log}::error "Error when communicating with plugwise: $line"
	    set PLUG(state) ERROR
	    break
	} elseif { [string match -nocase "*:*" $line] } {
	    foreach {k v} [split $line ":"] break
	    set k [string trim $k "{' \t\r\n"]
	    set v [string trim $v " \t\r\n',}"]
	    ::switch -glob -- $k {
		hw_ver -
		hz -
		last_logaddr {
		    set PLUG(nfo:$k) $v
		}
		fw_ver -
		datetime {
		    set PLUG(nfo:$k) [jsdate $v]
		}
		*state {
		    set PLUG(-state) [string is true $v]
		    ${log}::info "Plug $PLUG(mac) in state: $PLUG(-state)"
		}
	    }
	}
    }
    close $fd

    if { $PLUG(state) eq "ERROR" } {
	::event::generate $p Error
    } else {
	set PLUG(state) INITED
	set PLUG(errors) 0
	::event::generate $p Inited

	__pulse $p init
    }
}


proc ::plugwise::config { p args } {
    variable PWISE
    variable log

    if { ![::uobj::isa $p plug] } {
	return -code error "$p unknown or wrong type!"
    }
    upvar \#0 $p PLUG

    ::uobj::inherit PLUG OLD
    set result [eval ::uobj::config PLUG "-*" $args]
    
    if { $PLUG(state) eq "NONE" } {
	init $p
    } else {
	if { $OLD(-state) ne $PLUG(-state) && $PLUG(-state) ne "" } {
	    switch $p $PLUG(-state)
	}
    }

    if { $OLD(-frequency) ne $PLUG(-frequency) } {
	if { $PLUG(-frequency) > 0 } {
	    # (re)initialise the plug state poller whenever we change
	    # the check frequency.
	    __pulse $p init
	} else {
	    set PLUG(-frequency) $OLD(-frequency)
	}
    }

    if { [llength [::uobj::diff OLD PLUG]] > 0 } {
	# Only generate a Configure event if something as changed,
	# otherwise every access to the object's properties will
	# generate a configure event!
	::event::generate $p Configure
    }

    return $result
}


proc ::plugwise::new { mac args } {
    variable PWISE
    variable log

    set mac [mac $mac]
    if { $mac eq "" } {
	${log}::error "$mac is not a valid plugwise MAC address"
	return -code error "$mac invalid MAC address"
    }

    set p [::uobj::new [namespace current] plug]
    upvar \#0 $p PLUG

    set PLUG(self) $p
    set PLUG(mac) $mac;    # MAC address in ZigBee network
    set PLUG(state) NONE;  # State of the state machine
    set PLUG(poller) "";   # Identifier of polling command
    set PLUG(usage) {};    # Power usage over time
    set PLUG(errors) 0;    # Number of errors when polling state

    # Extract from PWISE the options which are not globals and should
    # be inherited by each object, i.e. by each plug.
    set options [list]
    foreach o [array get PWISE -*] {
	if { [lsearch $PWISE(globals) $o] < 0 } {
	    lappend options $o
	}
    }

    ::uobj::inherit PWISE PLUG $options
    ::uobj::objectify $p [list [list config configure] [list destroy delete] \
			      switch get init]
    eval config $p $args

    return $p
}

package provide plugwise $::plugwise::version