##################
## Module Name     --  csvout
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    The purpose of this module is to store the successive values of
##    objects into CSV files so historical data can easily be imported
##    in graphing tools or similar.
##
##################

# IMPLEMENTATION NOTES
#
# At initialisation, the module register a write trace on all the
# objects that have been created within the model. Every time a write
# occur, the trace will give the object a respit period (controlled by
# -flush, in milliseconds) and write that version of the object to the
# database (see below).  The respit allows for a number of successive
# operations within that (short!) time frame so as to try minimising
# the number of versions in the database.
#
# A timestamp and a description of what fields have been changed is
# automatically added to the CSV files in order to facilitate their
# import.  Future or past updates are ignored.

package require uobj
package require event

namespace eval ::csvout {
    variable CSVOUT
    if { ![info exists CSVOUT] } {
	array set CSVOUT {
	    -dir       ""
	    -flush     200
	    -fname     "%name%--%uuid%.csv"
	    -separator ","
	    -quote     "\""
	    -cutter    "|"
	    -format    "%Y%m%d-%H:%M:%S"
	    -header    on
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log csvout CSVOUT
	::uobj::install_defaults csvout CSVOUT
    }
}


# ::csvout::__dumpfield -- Convert value for CSV storage
#
#	Format a value, issuing from a field so that it can be stored
#	in the CSV database.  The procedure will replace object
#	identifiers by their UUIDs and booleans by 0 or 1.
#
# Arguments:
#	f	Identifier of the field spec in schema
#	val	Value to convert
#
# Results:
#	Return a string ready for insertion in a CSV file.
#
# Side Effects:
#	None.
proc ::csvout::__dumpfield { f val } {
    if { [$f get builtin] || [$f get constraint] ne "" } {
	# Can we convert UUID better to save memory?
	switch -nocase -- [$f type] {
	    String -
	    UUID -
	    Timestamp {
		return $val
	    }
	    Boolean {
		return [expr [string is true $val]?1:0]
	    }
	    default {
		return $val
	    }
	}
    } else {
	if { $val eq "" } {
	    return ""
	} else {
	    upvar \#0 $val OBJ
	    return $OBJ(uuid)
	}
    }
}



# ::csvout::__field -- Format field value for insertion in CSV file
#
#	Convert the content of a field for insertion in a CSV file.
#	This procedure understands properly multi-fields, i.e. arrays
#	of values.  Conversion of single values is delegated to
#	__dumpfield.
#
# Arguments:
#	f	Field specification from schema
#	val	Value to convert
#
# Results:
#	Return a string ready for insertion into CSV file
#
# Side Effects:
#	None.
proc ::csvout::__field { f val } {
    set result ""
    if { [$f get -multi] } {
	foreach v $val {
	    lappend result [__dumpfield $f $v]
	}
    } else {
	set result [__dumpfield $f $val]
    }
    return $result
}


# ::csvout::__open -- Open CSV file to append data
#
#	Open (possibly creating) a CSV file so as to be able to append
#	data to the file.  At creating, a header is automatically
#	added to the CSV file as specified in RFC4180.
#
# Arguments:
#	csvout	CSV context as created by new
#	o	Identifier of object to open file for data output
#
# Results:
#	Return a file descriptor ready for appending data, empty
#	string on file/directory creation errors.
#
# Side Effects:
#	None.
proc ::csvout::__open { csvout o } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    upvar \#0 $o OBJ
    set RESOLVER(name) $OBJ(name)
    set RESOLVER(ref) $OBJ(name)
    set RESOLVER(uuid) $OBJ(uuid)
    set RESOLVER(class) [::uobj::type $o]

    # Create directory for output, if it does not exist.
    set dir [::uobj::resolve $o $COUT(-dir) [array get RESOLVER]]
    if { [catch {file mkdir $dir} err] } {
	${log}::error "Cannot create/access directory $dir: $err"
	return ""
    }

    # Open file for appending, testing its existence before opening so
    # as to be able to automatically append a header at creation time.
    set fname [file join $dir \
		   [::uobj::resolve $o $COUT(-fname) [array get RESOLVER]]]
    set header [expr ![file exists $fname]]
    if { [catch {open $fname "a+"} fd] } {
	${log}::warn "Cannot open file at $fname for CSV output: $fd"
	set fd ""
    } else {
	if { $header && [string is true $COUT(-header)] } {
	    # Access the class directing the content of the object.
	    set c [[$COUT(model) get schema] find [::uobj::type $o]]
	    set fields [list __timestamp]
	    foreach s [$c inheritance on] {
		foreach f [$s get fields] {
		    lappend fields [__quoted $csvout [$f get -name]]
		}
	    }
	    lappend fields __hints
	    puts $fd "[join $fields $COUT(-separator)]"
	}
    }

    # Return file descriptor, caller should close
    return $fd
}


# ::csvout::__quoted -- Handle quotation of field contents
#
#	Quote a value as specified in RFC4180.
#
# Arguments:
#	csvout	CSV context as created by new
#	val	Value to quote
#
# Results:
#	Return quoted value.
#
# Side Effects:
#	None.
proc ::csvout::__quoted { csvout val } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    # Replace quotes by double quotes if any
    if { [string first $COUT(-quote) $val] >= 0 } {
	set val [string map $val \
		     [list $COUT(-quote) [string repeat $COUT(-quote) 2]]]
    }
    # Force quoting if we have separators or quotes
    if { [string first $COUT(-separator) $val] >= 0 \
	     || [string first $COUT(-quote) $val] >= 0 } {
	set val "$COUT(-quote)${val}$COUT(-quote)"
    }

    return $val
}


# ::csvout::__flush -- Flush a version to CSV file
#
#	Writes the current content of the object to the CSV file
#	implicitely under the current time as the version.
#
# Arguments:
#	csvout	CSV context created by <new>
#	o	Identfier of object.
#
# Results:
#	Return the list of fields that were found in the update.
#
# Side Effects:
#	Append textual representation of the update to the CSV file.
proc ::csvout::__flush { csvout o } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    # Ignored scheduled writes, empty the output timer and the hints. 
    if { [::uobj::keyword $o "when"] ne "" } {
	::uobj::keyword $o csv/output ""
	::uobj::keyword $o csv/hints ""
	return {}
    }

    # Open file for output, creating if necessary
    set fd [__open $csvout $o]

    # Access the class directing the content of the object.
    set c [[$COUT(model) get schema] find [::uobj::type $o]]

    # For all the fields coming from all the classes that we inherit
    # from, store the value in the file.  Make sure to prepend current
    # timestamp for the operation, and to append an hint over which
    # fields were modified by the operation, even though we actually
    # store the whole content of the object each time.
    set hints {}
    if { $c ne "" && $fd ne "" } {
	upvar \#0 $o OBJ
	set now [clock seconds]

	# Prepend timestamp
	set values [list [__quoted $csvout \
			      [clock format $now -format $COUT(-format)]]]
	
	# Continue with quoted content of all fields.
	foreach s [$c inheritance on] {
	    foreach f [$s get fields] {
		set fname [$f get -name]
		if { [array names OBJ -$fname] ne "" } {
		    lappend values [__quoted $csvout [__field $f $OBJ(-$fname)]]
		}
	    }
	}
	
	# Append an hint describing which fields were modified.
	foreach h [::uobj::keyword $o csv/hints] {
	    lappend hints [string trimleft $h "-"]
	}
	lappend values [__quoted $csvout [join $hints $COUT(-cutter)]]

	# Dump single line to CSV file
	puts $fd [join $values $COUT(-separator)]
    }
    
    # Close file, we will reopen next time if necessary.
    if { $fd ne "" } {
	close $fd
    }

    # Empty the output timer and the hints. 
    ::uobj::keyword $o csv/output ""
    ::uobj::keyword $o csv/hints ""

    return $hints
}


# ::csvout::__write -- Remember write and schedule versioning dump
#
#	This procedure is registered to be called each time an object
#	of the model is written to.  It schedule a version dump of the
#	object to the database in no more than -flush milliseconds.
#
# Arguments:
#	csvout	CSV context, as created by <new>
#	varname	Name of variable being changed.
#	idx	Index in array, (always relevant in our case)
#	op	Operation on object (always write in our case)
#
# Results:
#	None.
#
# Side Effects:
#	Schedule a version dump of the object to the relevant CSV file.
proc ::csvout::__write { csvout varname idx op } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT
    upvar $varname V; # Variable name is in the context of the calling
		      # procedure, i.e. where the write operation
		      # occurs.  We utilise the fact that we always
		      # store the identifier of the object under the
		      # index id, which is an undocumented feature.

    # Arrange to push data to the database within -flush milliseconds.
    set delay [::uobj::keyword $V(id) csv/output]
    if { $delay eq "" } {
	set delay [after $COUT(-flush) \
		       [namespace current]::__flush $csvout $V(id)]
	::uobj::keyword $V(id) csv/output $delay
    }

    # Remember what was changed in the object, make sure we do this
    # only once, no point otherwise...
    set hints [::uobj::keyword $V(id) csv/hints]
    lappend hints $idx
    ::uobj::keyword $V(id) csv/hints [lsort -unique $hints]
}


# ::csvout::__trace -- Setup traces on object write
#
#       Arrange to have internal traces on write on an object (of the
#       model) so that data from the object will be written to the
#       database automatically whenever the object is modified.
#
# Arguments:
#	csvout	CSV context, as created by <new>
#       o       Identifier of the object
#
# Results:
#       Return the identifier of the object if a trace was setup,
#       empty string if no trace was installed, which means that we
#       detected that there was already a trace from us on that
#       object.
#
# Side Effects:
#       Will arrange for content of the object updates to be written
#       to the database that is associated to the object.
proc ::csvout::__trace { csvout o } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    set found 0
    foreach nfo [trace info variable $o] {
	foreach {op pfx} $nfo break
	if { [string first [namespace current]::__write $pfx] >= 0 } {
	    set found 1
	}
    }

    # No trace was set up, install a write trace every time the
    # object is written.
    if { ! $found } {
	trace add variable $o write [list [namespace current]::__write $csvout]
	return $o
    }

    return ""
}


# ::csvout::config -- Configure CSV context object.
#
#	Configure or access values for a CSV output context object.
#
# Arguments:
#	csvout	CSV context, as created by <new>
#	args	List of dash-led options with values.
#
# Results:
#	Return the current value of an option if only one option was
#	given as an argument.
#
# Side Effects:
#	None.
proc ::csvout::config { csvout args } {
    variable CSVOUT
    variable log

    if { ![::uobj::isa $csvout csvout] } {
	return -code error "$csvout unkown or wrong type"
    }
    upvar \#0 $csvout COUT

    # Save prior content
    ::uobj::inherit COUT OLD
    set result [eval ::uobj::config COUT "-*" $args]

    # For all objects in the context, see if we have setup the traces
    # on write and arrange for setting them up if they are missing.
    set traced [list]
    foreach o [$COUT(model) get objects] {
	if { [__trace $csvout $o] ne "" } {
	    lappend traced $o
	}
    }
    ${log}::debug "Installed write traces on [join $traced , ]"

    return $result
}


# ::csvout::new -- Create new context for historical data storage
#
#	Create a new context for storage of object versions at within
#	a directory.  The context is bound to a model and the objects
#	that have been created within the model.  The current
#	implementation does not automatically detect new objects, but
#	this can be manually done by just calling the config
#	procedure.  The options (in args) that are accepted are the
#	following:
#       -dir     Directory (%-enclosed variables are allowed as of diskutil)
#       -flush   Time in milliseconds before actually attempting file write
#
# Arguments:
#	mdl	Identifier of a model
#	args	List of dash-led options and their values, see above.
#
# Results:
#	Return an identifier that is also a command, for Tk-style
#	access to the library.
#
# Side Effects:
#	None.
proc ::csvout::new { mdl args } {
    variable CSVOUT
    variable log

    set csvout [::uobj::new [namespace current] csvout]
    upvar \#0 $csvout COUT

    set COUT(self) $csvout;    # Ourselves
    set COUT(model) $mdl;      # The model that we are bound to

    # Listen to events on the schema to discover new objects that
    # would be created later.  This is essential to arrange for the
    # content of updates to be pushed into the database.
    ::event::bind [$mdl get schema] New \
	[list [namespace current]::__trace $csvout %i]

    ::uobj::inherit CSVOUT COUT
    ::uobj::objectify $csvout [list [list config configure]]
    
    eval config $csvout $args

    return $csvout
}

package provide csvout $::csvout::version
