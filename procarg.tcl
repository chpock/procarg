# procarg - parsing named arguments for procedures
# Copyright (C) 2018-2020 Kostiantyn Kushnir <chpock@gmail.com>
#
# See the file "LICENSE" for information on usage and redistribution of
# this file, and for a DISCLAIMER OF ALL WARRANTIES.

package provide procarg 1.0.1

namespace eval procarg {
  variable box
  variable paramtypes {string boolean integer double switch list dict args}
  variable regtypes
  array set regtypes [list]

  namespace export *
  namespace ensemble create
  set box [dict create ::procarg::registerkey [list \
      -default    [list string  ""     ""     ignore true  false false] \
      -nodefault  [list switch  ""     ""     false  true  false false] \
      -restrict   [list string  ""     ""     ignore false false false] \
      -allowempty [list boolean ""     ""     false  true  false false] \
      -stripdash  [list switch  false  ""     false  false false false] \
      -required   [list switch  false  ""     false  false false false] \
      __cache [list -restrict "" -stripdash false -required false] \
  ]]
}

proc procarg::getregistration { func } {
  variable box

  if { [string range $func 0 1] ne "::" } {
    if { ![catch [list uplevel 1 [list self call]] class] } {
      set class [lindex [lindex $class 0] [lindex $class 1]]
      set class [lindex $class 2]
      set func ${class}::$func
    } else {
      if { [set ns [uplevel 1 [list namespace current]]] eq "::" } {
        set func ::$func
      } else {
        set func ${ns}::[namespace tail $func]
      }
    }
  }

  if { ![dict exists $box $func] } { return -code error "arguments for $func not registered yet." }

  set o [dict get $box $func]
  dict unset o __cache

  return $o

}

proc procarg::regtype { type args } {
  variable regtypes
  variable paramtypes

  parse
  ##nagelfar variable opts array

  ##nagelfar ignore No braces around expression in if statement
  if { [info exists opts(-expression)] && [catch {set val 0; if $opts(-expression) {}} msg] } {
    return -code error "error while eval expression for custom argument type \"$type\": $msg"
  }
  if { [lsearch -exact $paramtypes $type] < 0 } { lappend paramtypes $type }
  if { ![info exists opts(-errormsg)] } { set opts(-errormsg) "is not correct ${type}." }
  set regtypes($type) [list $opts(-expression) $opts(-errormsg)]
}

proc procarg::registerkey { func key type args } {
  variable box
  variable paramtypes

  parse
  ##nagelfar variable opts array

  if { [set idx [lsearch -glob $paramtypes ${type}*]] < 0 } {
    return -code error "${func}: unknown type $type while registering arguments.\nAllowed types: [string trim $paramtypes]"
  }
  set type [lindex $paramtypes $idx]
  if { [string match {[0-9]} $key] } {
    if { $type eq "switch" } {
      return -code error "${func}: type $type not supported for internal proc arg <${key}>"
    }
    if { $opts(-stripdash) } {
      return -code error "${func}: -stripdash option not suported for internal proc arg <${key}>"
    } elseif { [info exists opts(-default)] } {
      return -code error "${func}: -default option not suported for internal proc arg <${key}>"
    }
  }
  if { $type eq "switch" } {
    set opts(-default) false
  }
  if { ![info exists opts(-allowempty)] } {
      if { $type in {string list dict} } {
          set opts(-allowempty) ignore
      } else {
          set opts(-allowempty) false
      }
  }
  # check restrict rules
  if { $opts(-restrict) ne "" && [lindex $opts(-restrict) 0] ne "script" } {
    switch -- $type {
      integer -
      double {
        foreach el $opts(-restrict) {
          if { [llength $el] == 1 } {
            ##nagelfar ignore Non static subcommand to
            if { ![string is $type -strict $el] } {
              return -code error "${func}: error in declaration restrict of \"$key\", \"$el\" is not ${type}."
            }
          } elseif { [llength $el] == 2 } {
            ##nagelfar ignore +3 Non static subcommand to
            ##nagelfar ignore Non static subcommand to
            if { ([set l [lindex $el 0]] ne "-" && ![string is $type -strict $l]) \
                  || ([set h [lindex $el 1]] ne "+" && ![string is $type -strict $h]) } {
              return -code error "${func}: error in declaration restrict of \"$key\" range \"$el\"."
            }
          } else {
            return -code error "${func}: error in declaration restrict of \"$key\" list size more then 2 \"$el\"."
          }
        }
      }
      list {
        lassign $opts(-restrict) l h
          if { $l in {in ni} } {
            if { [catch {llength $h} errmsg] } {
              return -code error "${func}: error in declaration restrict of \"$key\", \"$h\" is not correct list."
            }
          } else {
            if { $l eq "" || $l eq "-" } {
              set l 0
            } elseif { ![string is integer -strict $l] } {
              return -code error "${func}: error in declaration restrict of \"$key\", \"$l\" lower list size is not integer."
            }
            if { $h eq "" || $h eq "+" } {
              set h "+"
            } elseif { ![string is integer -strict $h] } {
              return -code error "${func}: error in declaration restrict of \"$key\", \"$l\" upper list size is not integer."
            }
          }
          set opts(-restrict) [list $l $h]
      }
      dict {
        return -code error "${func}: error in arg declaration \"$key\": -restrict option not supported for dict arg type."
      }
    }
  }
  # check default value (ignore proc arguments)
  if { [info exists opts(-default)] } {
    if { ![string match {[0-9]} $key] && [catch { checkvalue $key $opts(-default) $type $opts(-restrict) true } msg] } {
      return -code error "${func}: error in declaration of default arguments.\n$msg"
    }
    set opts(-nodefault) false
  } else {
    set opts(-default) ""
    set opts(-nodefault) true
  }
  dict set box $func $key [list $type $opts(-default) $opts(-restrict) $opts(-allowempty) $opts(-nodefault) $opts(-stripdash) $opts(-required)]
}

proc procarg::register { func params } {
  variable box
  variable paramtypes

  if { [string range $func 0 1] ne "::" } {
    if { [set ns [uplevel 1 [list namespace current]]] eq "::" } {
      set func ::$func
    } else {
      set func ${ns}::[namespace tail $func]
    }
  }

  foreach param $params {
    if { [llength $param] < 2 } {
      return -code error "${func}: error in declaration arguments \"argument type ?options?\": <${param}>"
    }
    if { [catch {registerkey {*}[concat [list $func] $param]} msg] } {
      return -code error $msg
    }
  }

  set cache [list]
  dict for { key val } [dict get $box $func] {
    if { [string match {[0-9]} $key] || $key eq "__cache" } continue
    if { [lindex $val 4] } continue
    if { [lindex $val 5] } {
      set key [string range $key 1 end]
    }
    lappend cache $key [lindex $val 1]
  }
  dict set box $func __cache $cache

  return ""
}

proc procarg::registerclass { class } {
  variable box

  if { [string range $class 0 1] ne "::" } {
    if { [set ns [uplevel 1 [list namespace current]]] eq "::" } {
      set class ::$class
    } else {
      set class ${ns}::[namespace tail $class]
    }
  }

  set methods [concat [info class methods $class -private] [list <constructor>]]

  foreach method $methods {

    set func "${class}::$method"

    if { ![dict exists $box $func] } {

      if { $method eq {<constructor>} } {
        if { [catch {
          lassign [info class constructor $class] targs tbody
        }] } continue
      } else {
        if { [catch {
          lassign [info class definition $class $method] targs tbody
        }] } continue
      }

      set xargs [list]
      set xbody ""

      foreach a $targs {
        if { [catch {llength $a} _] || $_ != 2 || [lindex $a 0] ne "args" } {
          if { [string match {&*} $a] } {
            append xbody "upvar 1 \${$a} [string range $a 1 end];"
          }
          lappend xargs $a
        } else {
          uplevel 1 [list ::procarg::register $func [lindex $a 1]]
          append xbody {::procarg::parse;}
          lappend xargs "args"
        }
      }

      if { $xbody eq "" } continue

      if { $method eq {<constructor>} } {
        ##nagelfar ignore Non constant definition {"$class".} Skipping.
        ::oo::define $class [list constructor $xargs [append xbody $tbody]]
      } else {
        ##nagelfar ignore +2 Non constant definition {"$class".} Skipping.
        ##nagelfar ignore Found constant {"method"} which is also a variable.
        ::oo::define $class [list method $method $xargs [append xbody $tbody]]
      }

    }

  }

}

proc procarg::parse { } {
  variable box

  upvar args a opts o

  if { [string range [set func [lindex [info level -1] 0]] 0 1] ne "::" } {
    if { [set ns [uplevel 1 [list namespace current]]] eq "::" } {
      set func ::$func
    } else {
      set func ${ns}::[namespace tail $func]
    }
  }

  if { ![catch [list uplevel 1 [list self call]] class] } {
    set class [lindex [lindex $class 0] [lindex $class 1]]
    set method [lindex $class 1]
    set class [lindex $class 2]
    set func ${class}::$method
    if { ![dict exists $box $func] } {
      if { $method eq {<constructor>} } {
        lassign [info class constructor $class] targs tbody
      } else {
        lassign [info class definition $class $method] targs tbody
      }
      set xargs [list]
      foreach targ $targs {
        if { [catch {llength $targ} _] || $_ != 2 || [lindex $targ 0] ne "args" } {
          lappend xargs $targ
        } else {
          uplevel 1 [list ::procarg::register ${class}::$method [lindex $targ 1]]
          lappend xargs "args"
        }
      }
      if { $method eq {<constructor>} } {
        ##nagelfar ignore Non constant definition {"$class".} Skipping.
        ::oo::define $class [list constructor $xargs $tbody]
      } else {
        ##nagelfar ignore +2 Non constant definition {"$class".} Skipping.
        ##nagelfar ignore Found constant {"method"} which is also a variable.
        ::oo::define $class [list method $method $xargs $tbody]
      }
      unset targs xargs tbody
    }
    unset method
  }

  if { ![dict exists $box $func] } { return -level 2 -code error "arguments for $func not registered yet." }

  array set o [dict get $box $func __cache]

  set tempkeys [dict get $box $func]
  dict unset tempkeys __cache

  if { [info exists a] } {
    for { set idx 0 } { $idx < [llength $a] } { incr idx } {
      set key [lindex $a $idx]
      if { ![dict exists $box $func $key] } { return -level 2 -code error "${func}: unknown option $key, must be one of: [dict keys [dict get $box $func] -*]" }
      lassign [dict get $box $func $key] type default restrict allowempty nodefault stripdash
      if { $type eq "args" } {
          lappend a {*}[lindex $a [incr idx]]
          continue
      } elseif { $type eq "switch" } {
        set val true
      } else {
        set val [lindex $a [incr idx]]
      }
      if { [catch {checkvalue $key $val $type $restrict $allowempty} msg] } {
        return -level 2 -code error "${func}: error while parse arguments\n$msg"
      }
      if { [dict exists $tempkeys $key] } {
        dict unset tempkeys $key
      }
      if { $stripdash } {
        set o([string range $key 1 end]) $val
      } else {
        set o($key) $val
      }
    }
  }

  dict for { key val } $tempkeys {
      # don't process native arguments
      if { [string match {[0-9]} $key] } continue
      # idx for 'required' flag is 6
      if { ![lindex $val 6] } continue
      set rkey $key
      # idx for 'stripdash' flag is 5
      if { [lindex $val 5] } {
          set rkey [string range $rkey 1 end]
      }
      if { ![info exists o($rkey)] } {
        return -level 2 -code error "${func}: error while parse arguments: required option '$key' is not specified"
      }
  }

  foreach idx [dict keys [dict get $box $func] {[0-9]}] {
    if { [set key [lindex [info args $func] $idx]] eq "" } {
      return -level 2 -code error "${func}: not found defined proc argument #${idx}."
    }
    set val [uplevel 1 [list set $key]]
    lassign [dict get $box $func $idx] type default restrict allowempty
    if { [catch {checkvalue $key $val $type $restrict $allowempty} msg] } {
      return -level 2 -code error "${func}: error while parse arguments\n$msg"
    }
  }

  return ""
}

proc procarg::checkvalue { key val type restrict allowempty } {
  variable regtypes
  # check for empty
  if { $allowempty ne "ignore" } {
    if { $allowempty && $val eq "" } { return $val }
    if { !$allowempty && $val eq "" } {
      return -code error "$key not allowed to be empty."
    }
  }
  # simple type check
  switch -- $type {
    boolean -
    integer -
    double {
      ##nagelfar ignore Non static subcommand to
      if { ![string is $type -strict $val] } {
        return -code error "$key \"$val\" is not $type value."
      }
    }
    list {
      if { [catch {llength $val} msg] } {
        return -code error "$key \"$val\" bad list - $msg"
      }
    }
    dict {
      if { [catch {dict size $val} msg] } {
        return -code error "$key \"$val\" bad dict - $msg"
      }
    }
  }
  # simple custom type check
  if { [info exists regtypes($type)] && [lindex $regtypes($type) 0] ne "" } {
    if { [catch {if { [eval [lindex $regtypes($type) 0]] } { set _ 0 } else { set _ 1 } } msg] } {
      return -code error "$key \"$val\" type check expression failed: $msg"
    } elseif { $msg } {
      return -code error "$key \"$val\" [lindex $regtypes($type) 1]"
    }
  }
  # check for restrictions
  if { $restrict eq "" } { return $val }
  if { [lindex $restrict 0] eq "script" } {
    if { [set expression [lindex $restrict 1]] ne "" } {
      ##nagelfar ignore No braces around expression in if statement.
      if { [catch {if $expression { set _ 0 } else { set _ 1 } } msg] } {
        return -code error "$key \"$val\" restrict expression failed: $msg"
      } elseif { $msg } {
        if { [set errstr [lindex $restrict 2]] eq "" } {
          set errstr "is not allowed."
        }
        return -code error "$key \"$val\" $errstr"
      }
    }
  } else {
    switch -- $type {
      string {
        if { [lsearch -exact $restrict $val] < 0 } {
          return -code error "$key \"$val\" is not in allowed values: [join [split $restrict] {, }]"
        }
      }
      double -
      integer {
        foreach el $restrict {
          if { [llength $el] == 1 } {
            if { $val == $el } { return $val }
          } elseif { [llength $el] == 2 } {
            if { [set l [lindex $el 0]] eq "-" } {
              if { $val <= [lindex $el 1] } { return $val }
            } elseif { [set h [lindex $el 1]] eq "+" } {
              if { $val >= $l } { return $val }
            } elseif { $val >= $l && $val <= $h } {
              return $val
            }
          }
        }
        return -code error "$key \"$val\" is not covered by allowed ranges: [join $restrict {, }]"
      }
      list {
        lassign $restrict l h
        if { $l eq "in" } {
          foreach el $val {
            if { $el ni $h } {
              return -code error "$key \"$el\" is not allowed list element"
            }
          }
        } elseif { $l eq "ni" } {
          foreach el $val {
            if { $el in $h } {
              return -code error "$key \"$el\" is not allowed list element"
            }
          }
        } elseif { [llength $val] < $l || ($h ne "+" && [llength $val] > $h) } {
          return -code error "$key \"$val\" is not covered by allowed range of list size: ${l}..${h}"
        }
      }
    }
  }

  return $val
}

procarg::register ::procarg::regtype {
  {-expression string -default ""}
  {-errormsg   string}
}

if { ![llength [info commands ::procarg::proc]] } {
  rename proc ::procarg::proc
}
if { ![llength [info commands ::procarg::oo_define]] } {
  rename ::oo::define ::procarg::oo_define
}

##nagelfar syntax ::procarg::proc dp

::procarg::proc proc { name arg body } {
  set xarg [list]
  if { [catch {llength $arg} _] || !$_ } {
    set xarg $arg
  } else {
    foreach a $arg {
      if { [catch {llength $a} _] || $_ != 2 || [lindex $a 0] ne "args" } {
        if { [string match {&*} $a] } {
          append xbody "upvar 1 \${$a} [string range $a 1 end];"
        }
        lappend xarg $a
      } else {
        uplevel 1 [list ::procarg::register $name [lindex $a 1]]
        append xbody {::procarg::parse;}
        lappend xarg "args"
      }
    }
  }
  ##nagelfar ignore Non constant argument to proc {"$name".} Skipping.
  tailcall ::procarg::proc $name $xarg [append xbody $body]
}

##nagelfar ignore Procedure {"oo::define"} does not match previous definition
proc ::oo::define { args } {
  set res [uplevel 1 [list ::procarg::oo_define {*}$args]]
  if { [lindex $args 0] ne "" } {
    uplevel 1 [list ::procarg::registerclass [lindex $args 0]]
  }
  return $res
}
