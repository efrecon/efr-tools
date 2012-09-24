##################
## Program Name    --  Plugwise
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    This program implements a duplex connection between a (set of)
##    plugwise plug(s) and the context engine.  Implemented on top of
##    the new WebSocket streaming interface, it bridges completely the
##    state of a physical plug and of an object within the context
##    engine.  Consequently, changes to the state of the physical plug
##    will automatically be reflected into the object in the context
##    engine; and (relevant) changes to the context object will be
##    propagated to the plug, turning on and off the relay that it
##    carries for example.
##
##    The implementation uses a library that itself is a wrapper
##    around the command line interface of the python library
##    available at https://bitbucket.org/hadara/python-plugwise/.
##    This require the command line program and library to be properly
##    installed.  Unless you change permissions yourself, you will
##    have to elevate your priviledges (sudo!) to run the Tcl script,
##    since it forks plugwise_util, which itself requires access to
##    the serial port (over USB).
##
##    When pushing data to the context manager, this process will
##    automatically "implement" a number of pieces of data. Each of
##    these can be specified as part of the links options.
##    Implemented are: sampling, which is reflects the -frequency of
##    the plug, though in milliseconds; power is the instant power
##    in W as read from the plug, it will only be updated when it
##    changes; energy will be the kW.h for the last entire hour (when
##    implemented); status is the state of the relay in the plug.
##
##################

package require Tcl

set options {
    { logfile.arg "%APPDATA%/me3gas/%progname%.log" "Where to save log for every run" }
    { context.arg "http://localhost:8802/" "Root URL to context manager" }
    { links.arg "729C24 5360eae6-a1cd-5e98-014f-127f395074f4 {status:status power energy sampling}" "List of plugise MAC matches to UUID and plug:field names" }
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


# ::dev:__send -- Send plug data to context manager
#
#       Sends plug data to the context manager. This procedure is
#       aware of the data types that are implemented by this process,
#       i.e. it automatically translates between the specified data
#       types to the names of the fields in the context manager, as
#       hinted at by the links options to the program.
#
# Arguments:
#	p	Indentifier of the plug object
#	arg1	descr
#	arg2	descr
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::dev:__send { p what value } {
    global PWISE

    if { [::uobj::isa $p plug] } {
	upvar \#0 $p PLUG

	foreach {plugField cxField} $PLUG(fields) {
	    if { [string equal -nocase $plugField $what] } {
		if { [string is integer $value] || [string is double $value] } {
		    set msg "\{\"$cxField\":$value\}"
		} else {
		    set msg "\{\"$cxField\":\"$value\"\}"
		}

		if { $PLUG(sock) ne "" } {
		    $PWISE(log)::notice "Sending $what = $cxField = $value to\
                                         object $PLUG(uuid)"
		    ::websocket::send $PLUG(sock) text $msg
		}
	    }
	}
    }
}


# ::dev:__plug -- Receive and dispatch plug data
#
#       Receives events from the plugwise library and dispatch
#       relevant data to the context manager to automatically reflect
#       the state of the plug in the object that is bound to it.
#
# Arguments:
#	p	Identifier of the local plug object
#	e	Name of the event that led to this call
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::dev:__plug { p e } {
    global PWISE

    if { [::uobj::isa $p plug] } {
	upvar \#0 $p PLUG

	switch $e {
	    Switch {
		# Forwards (new) state of plug as a JSON format to the
		# context manager along the websocket.
		set state [string is true [$PLUG(plug) get -state]]
		::dev:__send $p "status" $state
	    }
	    Change {
		set power [lindex [$PLUG(plug) get usage] end]
		::dev:__send $p "power" $power
	    }
	}
    }
}



# ::dev:__context -- Receive info from context manager
#
#       Handles event from the WebSocket connection to the context
#       manager, i.e. mainly incoming data whenever the content of the
#       objects that the plugs are bound to are modified.
#
# Arguments:
#	p	Identifier of the plug object.
#	sock	Identifier of the (web) socket to the context manager.
#	type	Type of the message received from the server
#	msg	Message content.
#
# Results:
#       None.
#
# Side Effects:
#       None.
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
		foreach {plugField cxField} $PLUG(fields) {
		    if { $plugField eq "status" \
			     && [dict exists $dta $cxField] } {
			set state [dict get $dta $cxField]
			if { [string is true $state] \
				 != [string is true \
					 [$PLUG(plug) get -state]] } {
			    $PLUG(plug) switch $state
			}
		    }
		}
	    }
	    "close" {
		# We should probably delete the plug object and (the
		# underlying) plug object from the plugwise library,
		# unless we want to automatically reopen (retry?) the
		# connection to the context manager in some way.
	    }
	    "connect" {
		# Associate the socket identifier to our local plug
		# object so we know where to send changes whenever the
		# state of the plug changes (right now: relay, soon
		# energy information).
		set PLUG(sock) $sock
		# Send current state and sampling rate to context manager.
		::dev:__plug $p Switch
		::dev:__send $p sampling \
		    [expr [$PLUG(plug) get -frequency]*1000]
	    }
	}
    }
}


# ::net:wsroot -- Return main context manager root URL
#
#       Sanitise the URL to the context manager, i.e. replace the http
#       scheme by ws, make it a web-socket URL (respecting the
#       trailing 's') and appending context if necessary.
#
# Arguments:
#       None.
#
# Results:
#       Return main URL to context manager, to be used as the entry
#       point to web sockets connections.
#
# Side Effects:
#       None.
proc ::net:wsroot {} {
    global PWISE

    # Find the scheme and replace http by ws, to make this a viable
    # websocket URL if necessary.
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

    # Make sure to append context after the root if not already
    # present.
    if { $root ne "" } {
	set root [string trimright $root "/"]
	if { [string range $root end-6 end] ne "context" } {
	    append root "/context"
	}
	return $root;  # We usually exit here, with a cool clean WS URL...
    }
    $PWISE(log)::error "Root of context should either be pointed at by HTTP\
                        or WS!"
    return ""
}


# ::dev:init -- Initialise bridge
#
#       Create a duplex bridge between a locally available PlugWise
#       plug and an object in the context manager.  Establish the
#       necessary event bindings to be able to propagate the state of
#       the plugs into the object and vice versa.
#
# Arguments:
#	mac	MAC address of the plug
#	uuid	UUID of the object in context
#	field	Name of field in object that contains the status
#
# Results:
#       Return the identifier of the local object representation of
#       the bridge, or an empty string on error.
#
# Side Effects:
#       None.
proc ::dev:init { mac uuid fields } {
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
	    ::event::bind $PLUG(plug) Switch "::dev:__plug $p %e"
	    ::event::bind $PLUG(plug) Change "::dev:__plug $p %e"
	    set PLUG(mac) [$plug get mac]
	    set PLUG(sock) ""
	    set PLUG(uuid) $uuid
	    set PLUG(token) [::websocket::open \
				 ${root}/${uuid}/stream \
				 [list ::dev:__context $p]]
	    set PLUG(fields) {}
	    foreach spec $fields {
		if { [string first ":" $spec] >= 0 } {
		    foreach {plugField cxField} [split $spec ":"] break
		    lappend PLUG(fields) $plugField $cxField
		} else {
		    lappend PLUG(fields) $spec $spec
		}
	    }
	}
    }

    return $p
}


# Initialise network, make sure we can support TLS encrypted
# connections.
package require tls
::http::register https 443 [list ::tls::socket]

# Create device listeners
foreach {mac uuid fields} $PWISE(links) {
    ::dev:init $mac $uuid $fields
}

vwait forever