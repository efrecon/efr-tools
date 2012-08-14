package require Tcl 8.2
package require logger

package require minihttpd

namespace eval ::minihttpd::websocket {
    variable WS
    if {![info exists WS]} {
	array set WS {
	    maxlength     16777216
	}
    }
}


proc ::minihttpd::websocket::__disconnect { sock } {
    variable WS

    set varname [namespace current]::Connection_$sock
    upvar \#0 $varname Connection

    catch {::close $sock}
    unset $varname
}


proc ::minihttpd::websocket::close { sock { reason {} } } {
    variable WS
    namespace upvar [namespace parent] log log

    set varname [namespace current]::Connection_$sock
    upvar \#0 $varname Connection

    if { $reason == "" } {
	puts -nonewline $sock \x88\x00
	${log}::info "Closing web socket"
    } else {
	set msg [binary format Su [lindex $reason 0]]
	append msg [encoding convertto utf-8 [lindex $reason 1]]
	set msg [string range $msg 0 124];  # Cut answer to make sure it fits!
	puts -nonewline $sock \x88[binary format c \
				       [string length $msg]]$msg
	${log}::info "Closing web socket: [lindex $reason 1]\
                      ([lindex $reason 0])"
    }

    __disconnect $sock
}


proc ::minihttpd::websocket::__push { sock type msg } {
    variable WS
    namespace upvar [namespace parent] log log

    set varname [namespace current]::Connection_$sock
    upvar \#0 $varname Connection

    if { [catch {$Connection(handler) $sock $type $msg} res] } {
	${log}::error "Error when executing WebSocket reception handler: $res"
    }
}


proc ::minihttpd::websocket::send { sock type {msg ""} {final 1}} {
    variable WS

    set varname [namespace current]::Connection_$sock
    upvar \#0 $varname Connection

    # Determine opcode from type, i.e. text, binary or ping.
    set opcode -1;
    if { [string is integer $type] } {
	set opcode $type
    } else {
	switch -glob -nocase -- $type {
	    t* {
		set opcode 1
	    }
	    b* {
		set opcode 2
	    }
	    p* {
		set opcode 9
	    }
	}
    }

    if { $opcode < 0 } {
	return -code error \
	    "Unrecognised type, should be one of text, binary, ping or\
             a protocol valid integer"
    }
    
    if { $Connection(write:opcode) > 0 } {
	if { $opcode != $Connection(write:opcode) } {
	    return -code error \
		"Cannot change type of message under continuation!"
	}
	set opcode 0;    # Continuation
    } else {
	set Connection(write:opcode) $opcode
    }

    # Encode text
    if { $Connection(write:opcode) == 1 } {
	set msg [encoding convertto utf-8 $msg]
    }

    # Reset continuation state once sending last fragment of message.
    if { $final } {
	set Connection(write:opcode) -1
    }

    # Assemble the header
    set header [binary format c [expr {!!$final << 7 | $opcode}]]
    if { [string length $msg] < 126 } {
	append header [binary format c [string length $msg]]
    } elseif { [string length $msg] < 65536 } {
	append header \x7e[binary format Su [string length $msg]]
    } else {
	append header \x7f[binary format Wu [string length $msg]]
    }

    # Send the frame
    puts -nonewline $sock $header$msg
    flush $sock
}



proc ::minihttpd::websocket::__receiver { sock } {
    variable WS
    namespace upvar [namespace parent] log log

    set varname [namespace current]::Connection_$sock
    upvar \#0 $varname Connection

    # Get basic header.  Abort if reserved bits are set, mask bit
    # isn't set, unexpected continuation frame, fragmented or
    # oversized control frame, or the opcode is unrecognized.
    binary scan [read $sock 2] Su header
    set opcode [expr {$header >> 8 & 0xf}]
    set mask [expr {$header >> 7 & 0x1}]
    set len [expr {$header &0x7f}]
    if { ($header & 0x7080 ^ 0x80) \
	     || ($opcode == 0 && $Connection(read:mode) eq "") \
	     || ($opcode > 7 && (!($header & 0x8000) || $len > 125)) \
	     || [lsearch {0 1 2 8 9 10} $opcode] < 0 } {
	# Send close frame, reason 1002: protocol error
	close $sock [list 1002 "Protocol Error"]
	return
    }

    # Determine the opcode for this frame, i.e. handle continuation of
    # frames.
    if { $Connection(read:mode) eq "" } {
	set Connection(read:mode) $opcode
    } elseif { $opcode == 0 } {
	set opcode $Connection(read:mode)
    }
    
    # Get the extended length, if present
    if { $len == 126 } {
	binary scan [read $sock 2] Su len
    } elseif { $len == 127 } {
	binary scan [read $sock 8] Wu len
    }

    # Limit the maximum message length
    if { [string length $Connection(read:msg)] + $len > $WS(maxlength) } {
	# Send close frame, reason 1009: frame too big
	close $sock [list 1009 "Limit $WS(maxlength) exceeded"]
	return
    }

    if { $opcode > 7 } {
	set oldmsg $Connection(read:msg)
	set Connection(read:msg) ""
    }

    if { $mask } {
	# Get mask and data.  Format data as a list of 32-bit integer
        # words and list of 8-bit integer byte leftovers.  Then unmask
	# data, recombine the words and bytes, and append to the buffer.
	binary scan [read $sock [expr {4+$len}]] \
	    II*c* mask words bytes
	set unmask_words {}
	set unmask_bytes {}
	for {set i 0} {$i < [llength $words]} {incr i} {
	    lappend unmask_words [expr {[lindex $words $i] ^ $mask}]
	}
	for {set i 0} {$i < [llength $bytes]} {incr i} {
	    lappend unmask_bytes [expr {[lindex $bytes $i] ^
					($mask >> (24 - 8 * $i))}]
	}
	append Connection(read:msg) \
	    [binary format I*c* $unmask_words $unmask_bytes]
    } else {
	binary scan [read $sock $len] c* bytes
	append Connection(read:msg) $bytes
    }

    # If the FIN bit is set, process the frame.
    if { $header & 0x8000 } {
	switch $opcode {
	    1 {
		# Text: decode and notify handler
		__push $sock text \
		    [encoding convertfrom utf-8 $Connection(read:msg)]
	    }
	    2 {
		# Binary: notify handler, no decoding
		__push $sock binary $Connection(read:msg)
	    }
	    8 {
		# Close: decode, notify handler and close frame.
		if { [string length $Connection(read:msg)] >= 2 } {
		    binary scan [string range $Connection(read:msg) 0 1] Su \
			reason
		    set msg [encoding convertfrom utf-8 \
				 [string range $Connection(read:msg) 2 end]]
		    __push $sock close [list $reason $msg]
		    close $sock [list $reason $msg]
		} else {
		    __push $sock close {}
		    close $sock 
		}
		return
	    }
	    9 {
		# Ping: send pong back...
		puts -nonewline $sock \
		    \x8a[binary format c \
			     [string length $Connection(read:msg)]]$msg
	    }
	}

	# Prepare for next frame.
	if { $opcode < 8 } {
	    # Reinitialise
	    set Connection(read:msg) ""
	    set Connection(read:mode) ""
	} else {
	    set Connection(read:msg) $oldmsg
	}
    }
}


proc ::minihttpd::websocket::handler { port sock handler } {
    variable WS

    set varname "[namespace parent [namespace current]]::Server_${port}"
    upvar \#0 $varname Server

    set varname [namespace current]::Connection_$sock
    upvar \#0 $varname Connection

    set Connection(sock) $sock
    set Connection(port) $port
    set Connection(handler) $handler

    set Connection(read:mode) ""
    set Connection(read:msg) ""
    set Connection(write:opcode) -1

    fconfigure $sock -translation binary -blocking on
    fileevent $sock readable [list [namespace current]::__receiver $sock]
}

package provide minihttpd::websocket 1.0