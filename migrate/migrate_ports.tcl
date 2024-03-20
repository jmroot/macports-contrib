#! /opt/local/libexec/macports/bin/tclsh8.6

package require macports
package require registry2

mportinit

if {![file writable $macports::portdbpath]} {
    ui_error "Insufficient privileges to write to MacPorts install prefix."
    exit 1
}

set ports_dryrun no
if {[lindex $argv 0] eq "-y"} {
    set ports_dryrun yes
}

proc deactivate {e} {
    set reg_options [dict create ports_nodepcheck 1]
    if {![registry::run_target $e deactivate $reg_options]
              && [catch {portimage::deactivate [$e name] [$e version] [$e revision] [$e variants] $reg_options} result]} {
        ui_debug $::errorInfo
        ui_error "Deactivating [$e name] @[$e version]_[$e revision][$e variants] failed: $result"
        exit 2
    }
}

proc deactivate_with_dependents {e} {
    if {[$e state] ne "installed"} {
        return
    }
    foreach dependent [$e dependents] {
        deactivate_with_dependents $dependent
    }
    deactivate $e
}

# Make the given entry and its dependendencies inactive if they were
# not originally active, but only if they don't now have dependents.
proc restore_inactive {e} {
    global statedict
    if {[dict exists $statedict [$e name] [$e requested_variants] installed] 
            || [$e state] ne "installed"} {
        return
    }
    foreach dependent [$e dependents] {
        if {[$dependent state] eq "installed"} {
            return
        }
    }
    deactivate $e
    foreach dep [$e dependencies] {
        restore_inactive $dep
    }
}

proc activate {e} {
    if {![registry::run_target $e activate {}]
              && [catch {portimage::activate [$e name] [$e version] [$e revision] [$e variants] {}} result]} {
        ui_debug $::errorInfo
        ui_error "Activating [$e name] @[$e version]_[$e revision][$e variants] failed: $result"
        return 0
    }
    return 1
}

proc splitvariants {variants} {
    set splitvariant [split $variants -]
    set minusvariant [lrange $splitvariant 1 end]
    set splitvariant [split [lindex $splitvariant 0] +]
    set plusvariant [lrange $splitvariant 1 end]
    set variations [dict create]
    foreach v $plusvariant {
        dict set variations $v +
    }
    foreach v $minusvariant {
        dict set variations $v -
    }
    return $variations
}

proc add_port_deps {portname requested_variants} {
    global portdeps mports install_status
    if {![dict exists $mports $portname $requested_variants]} {
        lassign [mportlookup $portname] portname portinfo
        if {[catch {mportopen [dict get $portinfo porturl] [dict create subport $portname] [splitvariants $requested_variants]} result]} {
            ui_debug $::errorInfo
            ui_error "mportopen for $portname $requested_variants failed: $result"
            dict set install_status $portname $requested_variants fail
            return
        } else {
            dict set mports $portname $requested_variants $result
        }
    }
    if {![dict exists $portdeps $portname $requested_variants]} {
        set mport [dict get $mports $portname $requested_variants]
        set portinfo [mportinfo $mport]
        set workername [ditem_key $mport workername]
        set deplist [list]
        foreach deptype {depends_fetch depends_extract depends_patch depends_build depends_lib depends_run} {
            if {[dict exists $portinfo $deptype]} {
                foreach depspec [dict get $portinfo $deptype] {
                    set dep_portname [$workername eval [list _get_dep_port $depspec]]
                    if {$dep_portname ne ""} {
                        lappend deplist $dep_portname
                    }
                }
            }
        }
        dict set portdeps $portname $requested_variants $deplist
    }
    foreach dep [dict get $portdeps $portname $requested_variants] {
        if {![dict exists $mports $dep]} {
            add_port_deps $dep {}
        }
    }
}

# Get a "good" set of requested variants to use with the given port.
proc get_good_variants {portname} {
    global statedict
    if {[dict exists $statedict $portname]} {
        set sub [dict get $statedict $portname]
        # Use variants from active version if there is one,
        # otherwise an arbitrary inactive version
        set best [dict filter $sub script {rv subsub} {
            expr {[dict keys $subsub] eq {installed}}
        }]
        if {[dict size $best] == 0} {
            set best $sub
        }
        return [lindex [dict keys $best] 0]
    }
    return {}
}

proc add_to_install_order {portname requested_variants} {
    global install_order_seen
    if {$requested_variants eq "*"} {
        set requested_variants [get_good_variants $portname]
    }
    #puts stderr "add_to_install_order $portname $requested_variants"
    if {[dict exists $install_order_seen [string tolower $portname] $requested_variants]} {
        return
    }

    global portdeps statedict active_install_list inactive_install_list
    # Mark seen before doing deps to guard against circular deps
    dict set install_order_seen [string tolower $portname] $requested_variants 1
    foreach dep [dict get $portdeps $portname $requested_variants] {
        add_to_install_order $dep *
    }

    # Add to the appropriate installation order, after its deps
    if {[dict exists $statedict $portname $requested_variants installed]} {
         lappend active_install_list $portname $requested_variants
    } elseif {[dict exists $statedict $portname $requested_variants imaged]} {
        lappend inactive_install_list $portname $requested_variants
    }
}

proc migrate_port {portname requested_variants} {
    global ports_dryrun mports portdeps depscache install_status statedict
    set mport [dict get $mports $portname $requested_variants]
    # Check for known_fail
    set portinfo [mportinfo $mport]
    if {[dict exists $portinfo known_fail] && [dict get $portinfo known_fail]} {
        ui_error "$portname $requested_variants is known to fail"
        dict set install_status $portname $requested_variants known_fail
        return
    }
    # Check if deps failed and skip if so
    foreach dep [dict get $portdeps $portname $requested_variants] {
        set dep_reg_entry [lindex [registry::entry installed $dep] 0]
        if {$dep_reg_entry ne "" && [dict exists $install_status $dep [$dep_reg_entry requested_variants]]
                && [dict get $install_status $dep [$dep_reg_entry requested_variants]] ne "ok"} {
            ui_error "Skipping $portname $requested_variants because its dependency $dep $rv failed"
            dict set install_status $portname $requested_variants dep_fail
            return
        }
        if {$dep_reg_entry eq {} && [dict exists $install_status $dep]} {
            dict for {rv status} [dict get $install_status $dep] {
                if {$status ne "ok"} {
                    ui_error "Skipping $portname $requested_variants because its dependency $dep $rv failed"
                    dict set install_status $portname $requested_variants dep_fail
                    return
                }
            }
        }
    }
    lassign [dict get $statedict $portname $requested_variants installed] version revision variants requested
    set reg_entry [lindex [registry::entry imaged $portname $version $revision $variants] 0]
    if {$ports_dryrun} {
        ui_msg "Skipping migration of $portname $requested_variants (dry run)"
        return
    }
    if {$reg_entry eq ""} {
        puts "$portname @${version}_${revision}${variants} doesn't seem to be installed"
        # Not installed, just try to install it
        if {[catch {mportexec $mport activate} result] || $result != 0} {
            dict set install_status $portname $requested_variants fail
        } else {
            # restore the requested flag
            set reg_entry [lindex [registry::entry installed $portname] 0]
            $reg_entry requested $requested
            dict set install_status $portname $requested_variants ok
        }
        return
    }
    # Activate the existing entry so upgrade will operate on the right one
    if {![activate $reg_entry]} {
        dict set install_status $portname $requested_variants fail
        return
    }
    set upgrade_options [macports::get_global_options]
    # We're doing this in dependency order, so no need to upgrade deps
    dict set upgrade_options ports_nodeps yes
    if {[macports::_mport_archs $mport] ne [$reg_entry archs]} {
        # Archs differ, so we have to force the upgrade
        dict set upgrade_options ports_upgrade_force yes
    }

    if {[catch {macports::upgrade $portname port:$portname {} $upgrade_options depscache} result] || $result != 0} {
        ui_error "upgrade $portname failed: $result"
        dict set install_status $portname $requested_variants fail
        return
    }
    # Mark success
    dict set install_status $portname $requested_variants ok
}

proc migrate_inactive_port {portname requested_variants} {
    global ports_dryrun mports portdeps install_status statedict
    set mport [dict get $mports $portname $requested_variants]
    # Check for known_fail, replaced_by
    set portinfo [mportinfo $mport]
    if {[dict exists $portinfo known_fail] && [dict get $portinfo known_fail]} {
        ui_error "$portname $requested_variants is known to fail"
        dict set install_status $portname $requested_variants known_fail
        return
    }
    if {[dict exists $portinfo replaced_by] && [dict get $portinfo replaced_by] ne ""} {
        ui_error "$portname $requested_variants is replaced by [dict get $portinfo replaced_by]"
        dict set install_status $portname $requested_variants replaced_by
        return
    }
    # Check if deps failed and skip if so
    foreach dep [dict get $portdeps $portname $requested_variants] {
        set dep_reg_entry [lindex [registry::entry installed $dep] 0]
        if {$dep_reg_entry ne "" && [dict exists $install_status $dep [$dep_reg_entry requested_variants]]
                && [dict get $install_status $dep [$dep_reg_entry requested_variants]] ne "ok"} {
            ui_error "Skipping $portname $requested_variants because its dependency $dep $rv failed"
            dict set install_status $portname $requested_variants dep_fail
            return
        }
        if {$dep_reg_entry eq {} && [dict exists $install_status $dep {}]} {
            if {[dict get $install_status $dep {}] ne "ok"} {
                ui_error "Skipping $portname $requested_variants because its dependency $dep $rv failed"
                dict set install_status $portname $requested_variants dep_fail
                return
            }
        }
    }

    lassign [dict get $statedict $portname $requested_variants imaged] version revision variants requested
    set reg_entry [lindex [registry::entry imaged $portname $version $revision $variants] 0]
    # Check if it needs migrating at all
    global macports::os_major macports::os_platform
    if {$reg_entry ne "" && [macports::_mport_archs $mport] eq [$reg_entry archs]
        && ($os_major eq [$reg_entry os_major] || [$reg_entry os_major] eq "any")
        && ($os_platform eq [$reg_entry os_platform] || [$reg_entry os_platform] eq "any")
    } then {
        ui_msg "No migration needed for $portname $requested_variants"
        return
    }

    if {$ports_dryrun} {
        ui_msg "Skipping migration of $portname $requested_variants (dry run)"
        return
    }
    # Clean in case a different variant was tried previously
    if {[catch {mportexec $mport clean} result] || $result != 0} {
        dict set install_status $portname $requested_variants fail
        return
    }
    if {![macports::global_option_isset ports_source_only]
            && ([catch {mportexec $mport archivefetch} result] || $result != 0)} {
        dict set install_status $portname $requested_variants fail
        return
    }
    if {![macports::global_option_isset ports_binary_only]
            && ([catch {mportexec $mport destroot} result] || $result != 0)} {
        dict set install_status $portname $requested_variants fail
        return
    }
    # Uninstall previous build
    set reg_options [dict create ports_force yes]
    if {![registry::run_target $reg_entry uninstall $reg_options]
        && [catch {registry_uninstall::uninstall $portname [dict get $portinfo version] [dict get $portinfo revision] [dict get $portinfo canonical_active_variants] $reg_options} result]} {
        dict set install_status $portname $requested_variants fail
        return
    }
    # Install without activating
    if {[catch {mportexec $mport install} result] || $result != 0} {
        dict set install_status $portname $requested_variants fail
        return
    }
    # restore the requested flag
    set reg_entry [lindex [registry::entry imaged $portname [dict get $portinfo version] [dict get $portinfo revision] [dict get $portinfo canonical_active_variants]] 0]
    $reg_entry requested $requested
    # mark success
    dict set install_status $portname $requested_variants ok
}

set install_status [dict create]
set portdeps [dict create]
set mports [dict create]

set all_reg_entries [registry::entry imaged]

set statefile ${macports::portdbpath}/migration.state
if {[file isfile $statefile]} {
    ui_msg "${macports::ui_prefix} Loading migration state from $statefile"
    set statefd [open $statefile r]
    set statedict [dict create {*}[read -nonewline $statefd]]
} else {
    set statedict [dict create]
    ui_msg "${macports::ui_prefix} Recording active ports"

    foreach ent $all_reg_entries {
        set requested_variants [$ent requested_variants]
        if {$requested_variants == 0} {
            set requested_variants {}
        }
        # always record active version if there is one, otherwise latest inactive
        if {![dict exists $statedict [$ent name] $requested_variants] || [$ent state] eq "installed"} {
            dict set statedict [$ent name] $requested_variants [$ent state] [list [$ent version] [$ent revision] [$ent variants] [$ent requested]]
            if {[$ent state] eq "installed"} {
                dict unset statedict [$ent name] $requested_variants imaged
            }
        } elseif {[$ent state] eq "imaged" && [dict exists $statedict [$ent name] $requested_variants imaged]
                && ![dict exists $statedict [$ent name] $requested_variants installed]} {
            lassign [dict get $statedict [$ent name] $requested_variants imaged] version revision
            if {[vercmp [$ent version] > $version] || ([vercmp [$ent version] == $version] && [$ent revision] > $revision)} {
                dict set statedict [$ent name] $requested_variants [$ent state] [list [$ent version] [$ent revision] [$ent variants] [$ent requested]]
            }
        }
    }
    set statefd [open $statefile w]
    puts -nonewline $statefd $statedict
}
close $statefd

ui_msg "${macports::ui_prefix} Loading Portfiles"
foreach ent $all_reg_entries {
    set entryname [$ent name]
    set requested_variants [$ent requested_variants]
    if {$requested_variants == 0} {
        set requested_variants {}
    }
    if {![dict exists mports $entryname $requested_variants]} {
        lassign [mportlookup $entryname] portname portinfo
        if {[catch {mportopen [dict get $portinfo porturl] [dict create subport $portname] [splitvariants $requested_variants]} result]} {
            ui_debug $::errorInfo
            ui_error "mportopen for $portname $requested_variants failed: $result"
            dict set install_status $portname $requested_variants fail
        } else {
            dict set mports $entryname $requested_variants $result
        }
    }
}

# Record dependencies for all installed ports
# (not all recursive deps are necessarily installed)
dict for {portname rvdict} $mports {
    dict for {rv mport} $rvdict {
        add_port_deps $portname $rv
    }
}

ui_msg "${macports::ui_prefix} Sorting ports into dependency order"
set install_order_seen [dict create]
set active_install_list [list]
set inactive_install_list [list]
dict for {portname rvdict} $mports {
    dict for {rv mport} $rvdict {
        add_to_install_order $portname $rv
    }
}

if {$ports_dryrun} {
    ui_msg "${macports::ui_prefix} Skipping deactivating active ports (dry run)"
} else {
    ui_msg "${macports::ui_prefix} Deactivating active ports"
    foreach ent [registry::entry installed] {
        deactivate_with_dependents $ent
    }
}

if {$ports_dryrun} {
    ui_msg "${macports::ui_prefix} Skipping cleaning ports (dry run)"
} else {
    ui_msg "${macports::ui_prefix} Cleaning ports"
    dict for {portname rvdict} $mports {
        dict for {rv mport} $rvdict {
            catch {mportexec $mport clean}
        }
    }
}

array set depscache {}
if {[llength $active_install_list] > 0} {
    ui_msg "${macports::ui_prefix} Migrating previously active ports"
    foreach {portname rv} $active_install_list {
        migrate_port $portname $rv
    }
} else {
    ui_msg "${macports::ui_prefix} No previously active ports to migrate"
}

if {[llength $inactive_install_list] > 0} {
    ui_msg "${macports::ui_prefix} Migrating previously inactive ports"
    foreach {portname rv} $inactive_install_list {
        migrate_inactive_port $portname $rv
    }
}

set new_ports [list]
if {$ports_dryrun} {
    ui_msg "${macports::ui_prefix} Skipping previous activation states (dry run)"
} else {
    ui_msg "${macports::ui_prefix} Restoring previous activation states"
    foreach ent [registry::entry installed] {
        if {[dict exists $statedict [$ent name] [$ent requested_variants] imaged]} {
            restore_inactive $ent
        }
    }
    foreach ent [registry::entry imaged] {
        # Record newly installed ports while we're at it
        if {![dict exists $statedict [$ent name] [$ent requested_variants]]} {
            lappend new_ports [$ent name] [$ent requested_variants]
            continue
        }
        if {[dict exists $statedict [$ent name] [$ent requested_variants] installed]} {
            set active_entry [registry::entry installed [$ent name]]
            if {$active_entry ne ""} {
                if {[$active_entry requested_variants] eq [$ent requested_variants]} {
                    continue
                } else {
                    deactivate $active_entry
                }
            }
            activate $ent
        }
    }
}

# Report what failed
set dep_failed_ports [list]
set failed_ports [list]
set known_fail_ports [list]
set replaced_ports [list]
#puts stderr $install_status
dict for {portname sub} $install_status {
    dict for {requested_variants status} $sub {
        switch -- $status {
            dep_fail {
                lappend dep_failed_ports $portname $requested_variants
            }
            fail {
                lappend failed_ports $portname $requested_variants
            }
            known_fail {
                lappend known_fail_ports $portname $requested_variants
            }
            replaced_by {
                lappend replaced_ports $portname $requested_variants
            }
        }
    }
}
if {[llength $replaced_ports] > 0} {
    ui_msg "The following ports were skipped because they have been replaced:"
    foreach {portname requested_variants} [lsort -dictionary -stride 2 $replaced_ports] {
        ui_msg "  $portname $requested_variants"
    }
}
if {[llength $known_fail_ports] > 0} {
    ui_msg "The following ports were skipped because they are known to fail:"
    foreach {portname requested_variants} [lsort -dictionary -stride 2 $known_fail_ports] {
        ui_msg "  $portname $requested_variants"
    }
}
if {[llength $failed_ports] > 0} {
    ui_msg "The following ports failed to install:"
    foreach {portname requested_variants} [lsort -dictionary -stride 2 $failed_ports] {
        ui_msg "  $portname $requested_variants"
    }
}
if {[llength $dep_failed_ports] > 0} {
    ui_msg "The following ports were skipped because one or more of their dependencies failed to install:"
    foreach {portname requested_variants} [lsort -dictionary -stride 2 $dep_failed_ports] {
        ui_msg "  $portname $requested_variants"
    }
}
if {[llength $new_ports] > 0} {
    ui_msg "The following new ports have been installed:"
    foreach {portname requested_variants} [lsort -dictionary -stride 2 $new_ports] {
        ui_msg "  $portname $requested_variants"
    }
}

if {[llength $failed_ports] > 0} {
    ui_msg "Not all ports migrated successfully."
    ui_msg "Migration starting state has been saved in '$statefile'."
    ui_msg "Running the migration script again will work based on the state file."
    ui_msg "Delete or rename this file if you wish to start a new migration from the current state."
} else {
    file delete $statefile
}

exit 0
