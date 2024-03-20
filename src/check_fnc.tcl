#!/usr/bin/tclsh
foreach source [glob *.f] {
    set fd [open $source r]
    while {[gets $fd line] != -1} {
	if [regexp -nocase\
		{^      (|[a-z0-9_]+ +)function +[a-z0-9_]+ *\(.*\)}\
		$line] {
	    regsub -nocase\
		    {^      (|[a-z0-9_]+ +)function +([a-z0-9_]+) *\(.*\)}\
		    $line {\2} name
	    lappend functions $name
	    puts "$source:\t$name"
	}
    }
    close $fd
}
foreach source [glob *f] {
    set fd [open $source r]
    set n 0
    while {[gets $fd line] != -1} {
	incr n
	foreach target $functions {
	    if [regexp -nocase "^(     | *\[0-9\]+).*call +$target" $line] {
		puts "$source:$n:$line"
	    }
	}
    }
    close $fd
}
