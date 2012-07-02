package require uri::urn


proc ::pair:extract { str } {
    set ids {}
    set idx 0
    while { [regexp -indices -start $idx {%\w+%} $str range] > 0 } {
	foreach {start stop} $range break
	lappend ids [string range $str [expr $start + 1] [expr $stop - 1]]
	set idx [expr $stop + 1]
    }

    return $ids
}


proc ::pair:receive { o r translations } {
    global CM

    upvar \#0 $o OBJ
    upvar \#0 $r REMOTE

    set uuid "local object"
    if { [array names OBJ uuid] ne "" } {
	append uuid " " $OBJ(uuid)
    }

    set touched_fields {}
    foreach {dst src} $translations {
	set dst_fields [pair:extract $dst]
	set dst_mapper {}
	foreach f $dst_fields {
	    lappend dst_mapper %$f% ""
	}
	
	set src_fields [pair:extract $src]
	set src_mapper {}
	foreach f $src_fields {
	    if { [array names REMOTE -$f] ne "" } {
		lappend src_mapper %$f% $REMOTE(-$f)
	    }
	    if { [array names REMOTE $f] ne "" } {
		lappend src_mapper %$f% $REMOTE($f)
	    }
	}

	if { [llength $dst_fields] == 1 \
		 && [string trim [string map $dst_mapper $dst]] eq ""} {
	    set df [lindex $dst_fields 0]
	    $CM(log)::notice "Copying sub-content of remote object into\
                              $uuid: ${df}=${src}"
	    if { [catch {expr [string map $src_mapper $src]} val] == 0} {
		set OBJ(-$df) $val
	    } else {
		set OBJ(-$df) [string map $src_mapper $src]
	    }
	    $CM(log)::debug "Copied sub-content from remote object into\
                             $uuid: ${df} = $OBJ(-$df)"
	    lappend touched_fields $df
	}
    }

    return $touched_fields
}


proc ::pair:init { fname pachkey } {
    global CM
    
    $CM(log)::notice "Opening $fname for pairing configuration"
    if { [catch {open $fname} fd] } {
	$CM(log)::error "Could not open $fname: $fd"
	return {}
    }

    set hex "\[a-f0-9\]"
    set uuid_filter "[string repeat $hex 8]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 4]-[string repeat $hex 12]"
    
    set pairs {}
    set lineno 0
    while { ![eof $fd] } {
	set line [gets $fd]
	incr lineno
	if { [string trim $line] ne "" } {
	    set first [string index $line 0]
	    if { [string first $first $CM(comments)] < 0 } {
		if { [string trimleft $line] ne $line } {
		    foreach {lft rgt} [split [string trim $line] "="] break
		    lappend PAIR(-translations) \
			[string trim $lft] [string trim $rgt]
		} else {
		    set p [::uobj::new [namespace current] pair]
		    upvar \#0 $p PAIR
		    foreach {from direction to} [string trim $line] break
		    set PAIR(-frequency) ""
		    regexp {\d+} $direction PAIR(-frequency)
		    set PAIR(id) $p
		    set PAIR(scheduler) ""
		
		    # Detect direction of pairing, we like writing an
		    # "arrow" in the file, e.g. --> but really only
		    # need the > or the < to detect the direction.
		    if { [string first ">" $direction] >= 0 } {
			set PAIR(-from) $from
			set PAIR(-to) $to
		    } else {
			set PAIR(-from) $to
			set PAIR(-to) $from
		    }
		    # Detect if we should force polling
		    if { [string first "!" $direction] >= 0 } {
			set PAIR(-polling) on
		    } else {
			set PAIR(-polling) off
		    }
		    set PAIR(-translations) {}

		    if { [string is integer $PAIR(-to)] \
			     || [string is integer $PAIR(-from)] } {
			set PAIR(-conduit) pachube
		    } elseif { [string range $PAIR(-to) 0 3] eq "gcal" \
				   || [string range $PAIR(-from) 0 3] \
				   eq "gcal"} {
			set PAIR(-conduit) gcal
		    } elseif { [string toupper [string range $PAIR(-to) 0 3]] eq "UPNP" \
				   || [string toupper [string range $PAIR(-from) 0 3]] \
				   eq "UPNP"} {
			set PAIR(-conduit) UPnP
		    } else {
			set PAIR(-conduit) remote
		    }

		    switch $PAIR(-conduit) {
			"pachube" {
			    if { [string is integer $PAIR(-to)] } {
				set PAIR(-destination) pachube
			    } else {
				set PAIR(-destination) context
			    }
			    set PAIR(-key) $pachkey
			}
			"UPnP" -
			"remote" -
			"gcal" {
			    if { [string match -nocase $uuid_filter \
				      $PAIR(-to)] } {
				set PAIR(-destination) context
			    } else {
				set PAIR(-destination) $PAIR(-conduit)
			    }
			}
		    }

		    lappend pairs $p
		}
	    }
	}
    }
    close $fd
    
    return $pairs
}


proc ::pair:register { p } {
    global CM

    upvar \#0 $p PAIR

    switch $PAIR(-conduit) {
	"pachube" {
	    if { $PAIR(-destination) eq "pachube" } {
		set feed $PAIR(-to)
		set uuid $PAIR(-from)
	    } else {
		set feed $PAIR(-from)
		set uuid $PAIR(-to)
	    }
	    set args \
		[list \
		     destination $PAIR(-destination) \
		     key $PAIR(-key) \
		     feed $feed \
		     translations $PAIR(-translations) \
		     polling $PAIR(-polling)]
	    if { $PAIR(-frequency) ne "" } {
		lappend args \
		    frequency $PAIR(-frequency)
	    }
	    ::api:pachube $uuid pair $args
	}
	"remote" {
	    if { $PAIR(-destination) eq "remote" } {
		set remote $PAIR(-to)
		set uuid $PAIR(-from)
	    } else {
		set remote $PAIR(-from)
		set uuid $PAIR(-to)
	    }
	    set args \
		[list \
		     destination $PAIR(-destination) \
		     remote $remote \
		     translations $PAIR(-translations) \
		     polling $PAIR(-polling)]
	    if { $PAIR(-frequency) ne "" } {
		lappend args \
		    frequency $PAIR(-frequency)
	    }
	    ::api:remote $uuid pair $args
	}
	"gcal" {
	    if { $PAIR(-destination) ne "context" } {
		$CM(log)::error "Cannot copy into gcal yet"
	    } else {
		if { [regexp {gcal://([a-zA-Z0-9\.\-%]+(\:[a-zA-Z0-9\.&%\$\-]+)*@)?(.*)(/)?} $PAIR(-from) match userpass pass cal] } {
		    set user [::uri::urn::unquote \
				  [lindex [split $userpass ":"] 0]]
		    set password [::uri::urn::unquote \
				      [string trimleft $pass ":"]]
		    set cal [::uri::urn::unquote \
				 [string trimright $cal "/"]]
		    set args [list \
				  user $user \
				  password $password \
				  calendar $cal]
		    if { $PAIR(-frequency) ne "" } {
			lappend args frequency $PAIR(-frequency)
		    }
		    ::api:gcal $PAIR(-to) pair $args
		} else {
		    $CM(log)::error "$PAIR(-from) is not a valid gcal URL spec"
		}
	    }
	}
	"UPnP" {
	    if { $PAIR(-destination) eq "UPnP" } {
		set UPnP $PAIR(-to)
		set uuid $PAIR(-from)
	    } else {
		set UPnP $PAIR(-from)
		set uuid $PAIR(-to)
	    }
	    # Get rid of "UPnP:" at the beginning and URI unquote to
	    # be able to support spaces.
	    set UPnP [string trimleft [string range $UPnP 4 end] ":"]
	    set UPnP [::uri::urn::unquote $UPnP]
	    set args [list \
			  device $UPnP \
			  destination $PAIR(-destination) \
			  translations $PAIR(-translations) \
			  polling $PAIR(-polling)]
	    if { $PAIR(-frequency) ne "" } {
		lappend args frequency $PAIR(-frequency)
	    }
	    ::api:UPnP $uuid pair $args
	}
    }
}
