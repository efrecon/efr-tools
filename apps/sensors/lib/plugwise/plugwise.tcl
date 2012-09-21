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
	    dft_mac_pfx  000D6F0000
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log plugwise PWISE
	::uobj::install_defaults plugwise PWISE
    }
}

# TODO
#
# Implement object destruction!!


# IMPLEMENTATION NOTES
# 
# Ideally, we wanted to use the -c option of the plugwise_util, but
# since it does not flush, the pipe does not contain anything until
# the process has ended, so we cannot get continuous data.  Instead we
# have to poll, starting a new process every xx seconds, and this for
# each plug.  To spread the load, the initial check for status is
# delayed using a random number so as to minimise the load on the
# resources.

proc ::plugwise::__mac { mac } {
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
	# Make a command that starts by changing the state and then
	# reads the current state to check that we were successful.
	set cmd "|\"$PWISE(-controller)\" -m $PLUG(mac) -d $PLUG(-dev)\
                  -s [string is true $state] -q relay_state"
	set fd [open $cmd RDWR]
	fconfigure $fd -buffering line -blocking 1 -translation lf
	set state [string trim [read $fd]]
	close $fd
	set PLUG(-state) $state
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
	mac {
	    return $PLUG(mac)
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

	while { ! [eof $fd] } {
	    set l [string trim [string tolower [gets $fd]]]
	    if { $l ne "" } {
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

    set when [expr {int($PLUG(-frequency)*1000)}]
    set PLUG(poller) [after $when [namespace current]::__check $p]
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


proc ::plugwise::__init { p } {
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
	if { [string match -nocase info* $line] } {
	} elseif { $line eq "" } {
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
		}
	    }
	}
    }
    close $fd

    set PLUG(state) INITED
    ::event::generate $p Inited

    if { $PLUG(poller) ne "" } {
	after cancel $PLUG(poller)
    }
    set when [expr {int(rand()*$PLUG(-frequency)*1000)}]
    set PLUG(poller) [after $when [namespace current]::__check $p]
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
	__init $p
    } else {
	if { $OLD(-state) ne $PLUG(-state) && $PLUG(-state) ne "" } {
	    switch $p $PLUG(-state)
	}
    }

    if { $OLD(-frequency) ne $PLUG(-frequency) } {
	if { $PLUG(-frequency) > 0 } {
	    # (re)state plugwise checker
	    if { $PLUG(poller) ne "" } {
		after cancel $PLUG(poller)
	    }
	    set when [expr {int(rand()*$PLUG(-frequency)*1000)}]
	    set PLUG(poller) [after $when [namespace current]::__check $p]
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

    set mac [__mac $mac]
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

    # Extract from PWISE the options which are not globals and should
    # be inherited by each object, i.e. by each plug.
    set options [list]
    foreach o [array get PWISE -*] {
	if { [lsearch $PWISE(globals) $o] < 0 } {
	    lappend options $o
	}
    }

    ::uobj::inherit PWISE PLUG $options
    ::uobj::objectify $p [list [list config configure] destroy switch get]
    eval config $p $args

    return $p
}

package provide plugwise $::plugwise::version