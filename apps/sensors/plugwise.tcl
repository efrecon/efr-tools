# Implement a plugwise bridge to the context manager.  This uses the
# WebSocket interface, both to put it to a test, but also because the
# connection needs to be bi-directional as plugs might get set from
# the outside.

# We must have at least 8.6 for IPv6 support!
package require Tcl

set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { context.arg "http://localhost:8802/" "Root URL to context manager" }
    { links.arg "729C24 5d9a66e5-9738-598c-d0b0-e707eb0e2a36 status" "List of plugise MAC matches to UUID and fieldName" }
}

array set PWISE {
    logfd       ""
    debug       0
}


source [file join [file dirname $argv0] lib init.tcl]
if { [string is true $PWISE(debug)] } {
    # Add debug port to something sensible and arrange to access
    # additional local libraries for when running from within a
    # dynamic debugger (RamDebugger).
    ::init::debughelper
}


# ::init:fix -- Fix packages depending on platform
#
#       This procedure is called during early initialisation and will,
#       on windows, arrange to load the registry package.  If a UI is
#       requested, it brings up a number of extra packages together
#       with a splash window that is shown until initialisation has
#       completed.
#
# Arguments:
#       glbl	Name of the global array, NP by construction
#       args	Arguments that have been passed to initialisation
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::init:fix { glbl args } {
    global PWISE
    global tcl_platform

    upvar \#0 $args ARGS
    if { $tcl_platform(platform) eq "windows" } {
	lappend ARGS(-packages) registry
    }
}


# ::log:out -- Output log to file and log window
#
#       This procedure is registered as a callback of the log module
#       and will both output to the log file and the log window,
#       whenever relevant.
#
# Arguments:
#       dt	Formated date
#       srv	Module within which the event occurs.
#       lvl	Level of severity
#       str	String description of the event
#
# Results:
#       Always return 1 so that the log module continues forwarding
#       the event to other registered procedures.
#
# Side Effects:
#       None.
proc log:out {dt srv lvl str} {
    global PWISE

    set logline "\[$dt\] \[$srv\] \[$lvl\] $str"

    # Return if no logfile specified
    if { $PWISE(logfile) eq "" } {
	return 1
    }

    if { $PWISE(logfd) eq "" } {
	set fname [file normalize [::diskutil::fname_resolv $PWISE(logfile)]]
	set d [file dirname $fname]
	if { ! [file isdirectory $d] } {
	    file mkdir $d
	}
	if { ! [catch {open $fname w+} fd] } {
	    set PWISE(logfd) $fd
	}
    }
    if { $PWISE(logfd) ne "" } {
	puts $PWISE(logfd) $logline
    }
    return 1
}


# Initialise everything in one go...
::init::init \
    -store PWISE \
    -options $options \
    -booleans [list] \
    -depends [list progver event rest http] \
    -packages [list rest uri base64 http] \
    -load [list plugwise websocket] \
    -parsed ::init:fix \
    -outlog ::log:out


# ::json:to_dict -- Convert JSON expression to a Tcl dictionary
#
#       This procedure converts a JSON expression to a Tcl
#       dictionary. It is ripped off from the wiki and is simpler (and
#       quicker) than the one that exists in the tcllib.
#
# Arguments:
#       json    JSON expression
#
# Results:
#       Returns a string that can be used as a dictionary.
#
# Side Effects:
#       None.
proc ::json:to_dict {json} {
    string range [
	string trim [
	    regsub -- {^(\uFEFF)} [
		string map {\t {} \n {} \r {} , { } : { } \[ \{ \] \}} $json
		] {}
	    ]
	] 1 end-1
}



# ::json:from_dict -- Convert a dictionary to a JSON expression
#
#	Convert a dictionary to a JSON expression, this is ripped off
#	the wiki and quicker, but not as complete as the implemenation
#	that comes with the JSON library.
#
# Arguments:
#	dctnary	Dictionary to traverse for conversion
#
# Results:
#	A well-formatted JSON expression
#
# Side Effects:
#	None.
proc ::json:from_dict {dctnary} {
    dict for {key value} $dctnary {
	if {[string match {\[*\]} $value]} {
	    lappend Result "\"$key\":$value"
	} elseif {![catch {dict size}]} {
	    lappend Result "\"$key\":\"[::json:from_dict $value]\""
	} else {
	    lappend Result "\"$key\":\"$value\""
	}
    }
    return "\{[join $Result ","]\}"
}


# Receives info (status changes) directly from plug
proc ::dev:__plug { p } {
    global PWISE

    if { [::uobj::isa $p plug] } {
	upvar \#0 $p PLUG

	# Forwards (new) state of plug as a JSON format to the context
	# manager along the websocket.
	set msg "\{\"$PLUG(field)\":[string is true [$PLUG(plug) get -state]]\}"
	if { $PLUG(sock) ne "" } {
	    ::websocket::send $PLUG(sock) text $msg
	}
    }
}


# Receives info (status) from context manager
proc ::dev:__context { p sock type msg } {
    global PWISE

    if { [::uobj::isa $p plug] } {
	upvar \#0 $p PLUG

	switch $type {
	    "text" {
		# Parse incoming data, i.e. get status of field in
		# object in context manager and switch the plug to
		# reflect the (new) state of the object.
		set dta [::json:to_dict $msg]
		if { [dict exists $dta $PLUG(field)] } {
		    set state [dict get $dta $PLUG(field)]
		    if { [string is true $state] \
			     != [string is true [$PLUG(plug) get -state]] } {
			$PLUG(plug) switch [dict get $dta $PLUG(field)]
		    }
		}
	    }
	    "close" {
	    }
	    "connect" {
		set PLUG(sock) $sock
		::dev:__plug $p;   # Send current state to cx manager
	    }
	}
    }
}


proc ::net:wsroot {} {
    global PWISE

    set root ""
    set colon [string first ":" $PWISE(context)]
    set scheme [string range $PWISE(context) 0 [expr {$colon-1}]]
    switch $scheme {
	"http" {
	    set root "ws:[string range $PWISE(context) [expr {$colon+1}] end]"
	}
	"https" {
	    set root "wss:[string range $PWISE(context) [expr {$colon+1}] end]"
	}
	"wss" -
	"ws" {
	    set root $PWISE(context)
	}
    }

    if { $root ne "" } {
	set root [string trimright $root "/"]
	if { [string range $root end-6 end] ne "context" } {
	    append root "/context"
	}
	return $root
    }
    $PWISE(log)::error "Root of context should either be pointed at by HTTP\
                        or WS!"
    return ""
}


proc ::dev:init { mac uuid field } {
    global PWISE

    set p [::uobj::find [namespace current] plug \
	       [list mac == [::plugwise::mac $mac]]]
    if { $p eq "" } {
	set plug [::plugwise::new $mac]
	set root [::net:wsroot]

	if { $plug ne "" && $root ne "" && [$plug get state] ne "ERROR" } {
	    set p [::uobj::new [namespace current] plug]
	    upvar \#0 $p PLUG
	    set PLUG(plug) $plug
	    ::event::bind $PLUG(plug) Switch "::dev:__plug $p"
	    set PLUG(mac) [$plug get mac]
	    set PLUG(sock) ""
	    set PLUG(token) [::websocket::open \
				 ${root}/${uuid}/stream \
				 [list ::dev:__context $p]]
	    set PLUG(field) $field
	}
    }

    return $p
}


proc ::net:init {} {
    global PWISE

    package require tls
    ::http::register https 443 [list ::tls::socket]
}

::net:init

# Create device listeners
foreach {mac uuid field} $PWISE(links) {
    ::dev:init $mac $uuid $field
}

vwait forever