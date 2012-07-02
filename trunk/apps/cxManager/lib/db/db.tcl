##################
## Module Name     --  db
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##    The purpose of this module is to store the successive instances
##    of objects that have been declared as part of a model in a DB
##    for further access.  The module also provides calls to get the
##    values of an object at given date and time (in the past).  The
##    module uses REDIS, an in-memory noSQL database for its storage,
##    which will allow for scalable data mining.  Each object can be
##    associated to a separate REDIS host for scalability.
##
##################

# IMPLEMENATION NOTES
#
# The module uses the main database and a key called global.databases
# to know where to store objects. It simply picks up the latest host
# present in the key (a list) when associating a (new) object to a
# database.
#
# At initialisation, the module register a write trace on all the
# objects that have been created within the model. Every time a write
# occur, the trace will give the object a respit period (controlled by
# -flush, in milliseconds) and write that version of the object to the
# database (see below).  The respit allows for a number of successive
# operations within that (short!) time frame so as to try minimising
# the number of versions in the database.
#
# Versions of objects are stored as a key formed by their UUID and the
# timestamp of the version. This is a hash and content is linearise so
# that it containts UUIDs instead of object identifiers.  Each object
# is also controlled by a sorted set (named after the UUID only),
# containing each timestamp weighted with the timestamp.  When looking
# for an object, the module will use zrangebyscore around the searched
# timestamp, benefiting from the range capabilities that are
# implemented in the sorted set at the database level.  The time span
# for searches is controlled by -span, which is in seconds and
# defaults to 24h.


package require uobj
package require event
package require redis

namespace eval ::db {
    variable DB
    if { ![info exists DB] } {
	array set DB {
	    -redis     localhost:6379
	    -flush     200
	    -span      86400
	}
	variable version 0.1
	variable libdir [file dirname [file normalize [info script]]]
	::uobj::install_log db DB
	::uobj::install_defaults db DB
    }
}


# ::db::dbopen -- Open a remote REDIS database
#
#	Open the connection to a remote REDIS database if it is hasn't
#	been opened yet and return the identifier of the this
#	connection within the database context.  The server should be
#	specified as the hostname (or IP address) separated from the
#	port number by a colon.  If the port number is omitted, the
#	default port number (as specified in the global DB array) will
#	be taken.
#
# Arguments:
#	db	Database context
#	server	Server in the form hostname:port
#
# Results:
#	Return the identifier of the REDIS connection
#
# Side Effects:
#	Attempts to open a socket to the REDIS remote database via the
#	redis package.
proc ::db::dbopen { db server } {
    variable DB
    variable log

    # Take default port from main object if none specified.
    foreach { host port } [split $server ":"] break
    if { $port eq "" } {
	foreach { - port } [split $DB(-redis) ":"] break
    }
    set redis [::uobj::find [namespace current] redis \
		   [list host == $host port == $port] [::uobj::id $db]]
    if { $redis eq "" } {
	set redis [::uobj::new [namespace current] redis [::uobj::id $db]]
	upvar \#0 $redis REDIS
	set REDIS(id) $redis
	set REDIS(redis) [redis $host $port]
	set REDIS(host) $host
	set REDIS(port) $port
	${log}::info "Opened connection to REDIS at ${host}:${port}"
    }

    return $redis
}


# ::db::__dumpfield -- Convert value for REDIS storage
#
#	Format a value, issuing from a field so that it can be stored
#	in the REDIS database.  The procedure will replace object
#	identifiers by their UUIDs and booleans by 0 or 1.
#
# Arguments:
#	f	Identifier of the field spec in schema
#	val	Value to convert
#
# Results:
#	Return a string ready for insertion in REDIS.
#
# Side Effects:
#	None.
proc ::db::__dumpfield { f val } {
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



# ::db::__field -- Format field value for insertion in REDIS
#
#	Convert the content of a field for insertion in REDIS.  This
#	procedure understands properly multi-fields, i.e. arrays of
#	values.  Conversion of single values is delegated to
#	__dumpfield.
#
# Arguments:
#	f	Field specification from schema
#	val	Value to convert
#
# Results:
#	Return a string ready for insertion into REDIS
#
# Side Effects:
#	None.
proc ::db::__field { f val } {
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



# ::db::__flush -- Flush a version to database
#
#	Writes the current content of the object to the REDIS
#	database, implicitely under the current time as the version.
#
# Arguments:
#	db	Database context created by <new>
#	o	Identfier of object.
#
# Results:
#	None.
#
# Side Effects:
#	None.
proc ::db::__flush { db o } {
    variable DB
    variable log

    if { ![::uobj::isa $db db] } {
	return -code error "$db unkown or wrong type"
    }
    upvar \#0 $db D

    # Allocate database to object if necessary
    set redis [__redis $db $o]
    upvar \#0 $redis REDIS

    # XXX: What to do about the hints?

    # Access the class directing the content of the object.
    set c [[$D(model) get schema] find [::uobj::type $o]]

    # For all the fields coming from all the classes that we inherit
    # from, store the value in REDIS at UUID.now where now is the
    # present time.  Remember that we have a timestamp for that UUID
    # by adding now to the sorted set UUID, with the time as the
    # score.  Using the time as the score will help us getting data
    # out of the DB in a scalable way.
    if { $c ne "" } {
	upvar \#0 $o OBJ
	set now [clock seconds]
	$REDIS(redis) deferred 1
	$REDIS(redis) zadd $OBJ(uuid) $now $now
	foreach s [$c inheritance on] {
	    foreach f [$s get fields] {
		set fname [$f get -name]
		if { [array names OBJ -$fname] ne "" } {
		    $REDIS(redis) hset $OBJ(uuid).$now \
			$fname [__field $f $OBJ(-$fname)]
		}
	    }
	}
	$REDIS(redis) deferred 0
    }

    # Empty the output timer and the hints. 
    ::uobj::keyword $o output ""
    ::uobj::keyword $o hints ""
}



# ::db::get -- Get version out of DB
#
#	Access the version that is closest to the timestamp provided
#	as a parameter from the database.  The implementation uses the
#	-span option of the database context when looking for
#	timestamps that might match, thus could miss some value if no
#	hit can be found within the span.
#
# Arguments:
#	db	Database context as returned by <new>
#	o	Object identifier
#	when	Time at which we want the version
#	resolv	Resolv UUIDs to identifiers
#
# Results:
#	Return a list ready for an array set command, empty if no
#	version for that object could be found.
#
# Side Effects:
#	Access the REDIS database associated to the object.
proc ::db::get { db o { when "" } { resolv on } } {
    variable DB
    variable log

    if { ![::uobj::isa $db db] } {
	return -code error "$db unkown or wrong type"
    }
    upvar \#0 $db D
    upvar \#0 $o OBJ

    # Access the class directing the content of the object.
    set c [[$D(model) get schema] find [::uobj::type $o]]
    
    if { $c ne "" } {
	# Access to the REDIS database object that contains the object.
	set redis [__redis $db $o]

	# Empty when is now, which really means latest. Maybe should
	# we take the value directly in that case?
	if { $when eq "" } {
	    set when [clock seconds]
	}
	
	# Get all existing timestamps around the time at which we want
	# the value and find the one that is the closest to the time
	# that we are requesting.
	upvar \#0 $redis REDIS
	set timestamps [$REDIS(redis) zrangebyscore $OBJ(uuid) \
			    [expr {$when-$D(-span)}] [expr {$when+$D(-span)}]]
	set timestamp "";  # Will contain closest timestamp
	foreach ts $timestamps {
	    if { $when >= $ts } {
		set timestamp $ts
	    }
	}
	
	# Now get the value of the object from the database at that
	# time, resolv the UUIDs to object instances and return.
	if { $timestamp ne "" } {
	    array set THEN [$REDIS(redis) hgetall $OBJ(uuid).$timestamp]
	    array set RETURN {}
	    if { [string is true $resolv] } {
		foreach s [$c inheritance on] {
		    foreach f [$s get fields] {
			set fname [$f get -name]
			if { [array names THEN $fname] ne "" } {
			    if { [$f get class] ne "" } {
				# Convert back UUIDs, as stored in
				# REDIS, into local object
				# identifiers.
				if { [$f get -multi] } {
				    set RETURN(-$fname) {}
				    foreach v $THEN($fname) {
					set vo [$D(model) find $v]
					if { $vo ne "" } {
					    lappend RETURN(-$fname) $vo
					}
				    }
				} else {
				    set RETURN(-$fname) \
					[$D(model) find $THEN($fname)]
				}
			    } else {
				set RETURN(-$fname) $THEN($fname)
			    }
			}
		    }
		}
	    } else {
		foreach k [array names THEN] {
		    set RETURN(-$k) $THEN($k)
		}
	    }
	    return [array get RETURN]
	}
    }

    return {}
}



# ::db::__redis -- Associate a database to an object.
#
#	Associate a database context for an object, and allocate a
#	database host to that object if necessary.
#
# Arguments:
#	db      Database context, as returned by <new>
#	o	Object identifier
#
# Results:
#	Return the identifier of an internal object that will contain
#	information about the database context for that object.
#
# Side Effects:
#	Allocates database host to object and store this in the main
#	REDIS database, i.e. the one that was specified at context
#	creation time in <new>.
proc ::db::__redis { db o } {
    variable DB
    variable log

    if { ![::uobj::isa $db db] } {
	return -code error "$db unkown or wrong type"
    }
    upvar \#0 $db D
    upvar \#0 $o OBJ

    set redis [::uobj::keyword $OBJ(id) redis]
    if { $redis eq "" } {
	${log}::debug "No REDIS database associated to object $OBJ(id) yet,\
                       picking up last used one"
	upvar \#0 $D(redis) MAIN
	set server [$MAIN(redis) hget global.database.ids $OBJ(uuid)]
	if { $server eq "" } {
	    # No server, pick the last one amongst the databases that
	    # we know of.
	    set server [lindex [$MAIN(redis) smembers global.databases] end]
	    $MAIN(redis) hset global.database.ids $OBJ(uuid) $server
	    $MAIN(redis) sadd global.database.$server $OBJ(uuid)
	    ${log}::info "Associated object id $OBJ(uuid) to redis server\
                          $server"
	}
	set redis [dbopen $db $server]
	::uobj::keyword $OBJ(id) redis $redis
    }

    return $redis
}


proc ::db::settle { db varname idx val { now "" } } {
    variable DB
    variable log

    if { ![::uobj::isa $db db] } {
	return -code error "$db unkown or wrong type"
    }
    upvar \#0 $db D

    if { $now eq "" } {
	set now [clock seconds]
    }

    # Push the operation in a queue, register __flush to be called
    # very soon.  The queue should contain lists of triplets,
    # i.e. identifier of the object, index to be set, value to be
    # pushed (possibly type of the operation, i.e. SET or APPEND?)

    # Modify __flush so that it is able to handle the queue above,
    # i.e. it either takes the value of the object from the object
    # itself (default) or from the different operations that have been
    # posted to the queue.

    # How do we handle lappend?

    # How do we store and hint this, i.e. setting values in the future
    # (since this is what we use this for).

    # How ::db::set gets registered to be called when we call
    # ::rest:set? This should be context dependent, i.e. if we have a
    # DB, then call otherwise do nothing?
}


# ::db::__write -- Remember write and schedule versioning dump
#
#	This procedure is registered to be called each time an object
#	of the model is written to.  It schedule a version dump of the
#	object to the database in no more than -flush milliseconds.
#
# Arguments:
#	db	Database context, as created by <new>
#	varname	Name of variable being changed.
#	idx	Index in array, (always relevant in our case)
#	op	Operation on object (always write in our case)
#
# Results:
#	None.
#
# Side Effects:
#	Schedule a version dump of the object to the relevant REDIS db.
proc ::db::__write { db varname idx op } {
    variable DB
    variable log

    if { ![::uobj::isa $db db] } {
	return -code error "$db unkown or wrong type"
    }
    upvar \#0 $db D
    upvar $varname V; # Variable name is in the context of the calling
		      # procedure, i.e. where the write operation
		      # occurs.  We utilise the fact that we always
		      # store the identifier of the object under the
		      # index id, which is an undocumented feature.

    # Arrange to push data to the database within -flush milliseconds.
    set delay [::uobj::keyword $V(id) output]
    if { $delay eq "" } {
	set delay [after $D(-flush) \
		       [namespace current]::__flush $db $V(id)]
	::uobj::keyword $V(id) output $delay
    }

    # Remember what was changed in the object, make sure we do this
    # only once, no point otherwise...
    set hints [::uobj::keyword $V(id) hints]
    lappend hints $idx
    ::uobj::keyword $V(id) hints [lsort -unique $hints]
}



# ::db::config -- Configure database context object.
#
#	Configure or access values for a database context object.
#	This will open the connection to the main REDIS database for
#	use within the module, if necessary.
#
# Arguments:
#	db	Database context, as created by <new>
#	args	List of dash-led options with values.
#
# Results:
#	Return the current value of an option if only one option was
#	given as an argument.
#
# Side Effects:
#	Close connection to (old) databases when the main redis
#	database changes.
proc ::db::config { db args } {
    variable DB
    variable log

    if { ![::uobj::isa $db db] } {
	return -code error "$db unkown or wrong type"
    }
    upvar \#0 $db D

    # Save prior content
    ::uobj::inherit D OLD
    set result [eval ::uobj::config D "-*" $args]

    # If the main REDIS database has changed, close connection to the
    # old one.
    if { $OLD(-redis) ne $D(-redis) } {
	if { $D(redis) ne "" } {
	    upvar \#0 $D(redis) REDIS
	    $REDIS(redis) close
	    ::uobj::delete $D(redis)
	    set D(redis) ""
	}
    }

    # No connection to REDIS yet, open it up. 
    if { $D(redis) eq "" } {
	set redis [dbopen $db $D(-redis)]
	# First time initialisation of the database if necessary.
	upvar \#0 $redis REDIS
	set servers [$REDIS(redis) smembers global.databases]
	if { [llength $servers] == 0 } {
	    ${log}::notice "First time initialisation of 'global.databases' key"
	    $REDIS(redis) sadd global.databases $REDIS(host):$REDIS(port)
	}
	
	# Remember main REDIS database in main DB context
	set D(redis) $redis
    }

    # For all objects in the context, see if we have setup the traces
    # on write and arrange for setting them up if they are missing.
    set traced [list]
    foreach o [$D(model) get objects] {
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
	    trace add variable $o write [list [namespace current]::__write $db]
	    lappend traced $o
	}
    }
    ${log}::debug "Installed write traces on $traced"

    return $result
}



# ::db::new -- Create new DB context for storage at REDIS
#
#	Create a new database context for storage of object version at
#	the REDIS database.  The context is bound to a model and the
#	objects that have been created within the model.  The current
#	implementation does not automatically detect new objects, but
#	this can be manually done by just calling the config
#	procedure.  The options (in args) that are accepted are the
#	following:
#       -redis   Main redis database, <hostname>:<port> formatted.
#       -flush   Time in milliseconds before actually attempting REDIS write
#       -span    Time in seconds when searching for objects at given versions.
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
proc ::db::new { mdl args } {
    variable DB
    variable log

    set db [::uobj::new [namespace current] db]
    upvar \#0 $db D

    set D(self) $db;    # Ourselves
    set D(model) $mdl;  # The model that we are bound to
    set D(redis) "";    # Connection to main REDIS database

    ::uobj::inherit DB D
    ::uobj::objectify $db [list [list config configure] get [list settle set]]
    
    eval config $db $args

    return $db
}

package provide db $::db::version