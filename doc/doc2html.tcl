#!/usr/bin/tclsh

set fi [open topdrawer.doc r]
set DIR html
if ![file isdirectory $DIR] {exec mkdir $DIR}
set fc [open $DIR/contents.html w]
puts $fc "<HEAD>\n<TITLE>"
puts $fc "Topdrawer Reference Manual -- Contents"
puts $fc "</TITLE>\n</HEAD>\n<BODY>"

set fo [open $DIR/index.html w]
puts $fo "<HEAD><TITLE>Topdrawer Reference Manual</TITLE></HEAD>
<FRAMESET COLS=\"30%,70%\">
      <FRAME SRC=\"contents.html\" NAME=\"Contents\">
      <FRAME SRC=\"notice.html\" NAME=\"Results\">
</FRAMESET>
<NOFRAME>
<H3>Topdrawer Refernce Manual</H3><P>
The new version of this page requires Netscape 2.0 or later.  
Click <FONT SIZE=+2><A HREF=\"old_index.html\">HERE</A></FONT> 
to access another version which will work with any browsers.
</NOFRAME>"
close $fo

set fo [open $DIR/notice.html w]
puts $fo "<HEAD>\n<TITLE>"
puts $fo "Topdrawer Reference Manual -- Notice"
puts $fo "</TITLE>\n</HEAD>\n<BODY>"
puts $fo "<H2><I>Topdrawer</I></H2>"
puts $fo "NOTE:  
This is an automatic translation from \"<CODE>TOPDRAWER.DOC</CODE>\", 
the original reference manual of the VMS-verion.  
Some features are lost in the port to UNIX made at FNAL, 
although most of them are restored in the 
<A HREF=\"http://iris.riken.go.jp/iris/topdrawer/topdrawer.txt\">
present port</A>.  
The enhanced features, such as the extended fonts and the symbols, 
are described in \"README.Linux\".  
Some of them will be demonstrated by \"<CODE>whatsnew.top</CODE>\" 
in the directory \"<CODE>examples</CODE>\""
puts $fo "<BR><HR><PRE>\n"

set size H4
set U U ; # underline
set page 0
set Index 0
proc Tail_Header {section {tag {}} {comment {}}} {
    global fo DIR size
    switch -regexp -- $section {
	{^[0-9]+$} {set title "Chapter $section"}
	{Index} {set title "Index"}
	default {set title "$section"}
    }
    puts  $fo "</PRE>\n</BODY>"
    close $fo
    set fo [open $DIR/$section.html w]
    puts $fo "<HEAD>"
    puts $fo "<TITLE>Topdrawer Reference Manual $title</TITLE>"
    puts $fo "</HEAD>"
    puts $fo "\n<BODY>\n<PRE>\n<B><$size>$title  $tag</$size></B>$comment"
}
while 1 {
    for {set i 0} {$i < 66} {incr i} {if {[gets $fi line($i)] == -1} break}
    puts "Page [incr page]\033M"
    if {$i} {
	if [regexp {^ +[0-9]+ *$} $line(14)] {
	    set tag  [string trim $line(16)]
	    set chap [string trim $line(14)]
	    Tail_Header $chap $tag
	    set i0 19
	} {
	    set i0 5
	    if [regexp {^ +Index *$} $line(10)] {
		set Index 1
		Tail_Header Index
		set i0 15
	    }
	    if [regexp {^ +Table of Contents *$} $line(9)] {
		puts $fc "<B><$size>Topdrawer Refernce Manual<BR>"
		puts $fc "-- Table of Contents</$size></B>\n<PRE>"
		puts $fc "<A HREF=\"notice.html\" TARGET=\"Results\">"
		puts $fc "<B>NOTICE</B></A>"
		set i0 12
	    }
	}
	set n 62
	while {$line([expr $n-2]) == ""} {incr n -1}
	for {set i $i0} {$i <= $n} {incr i} {
	    set a $line($i)
	    if [regexp {^[ \-]+$} $line([expr $i+1])] {
		if ![regexp {^     ([0-9.]+) +.+$} $a] {
		    set c "$line([expr $i+1]) "
		    for {set j [expr [string length $c]-2]}\
			    {$j > 0} {incr j -1} {
			switch -- [string range $c $j [expr $j+1]] {
			    "- " {set a "[string range $a 0 $j]</$U>[string range $a [expr $j+1] end]"}
			    " -" {set a "[string range $a 0 $j]<$U>[string range $a [expr $j+1] end]"}
			}
			if {[string index $a $j] == "<"}\
				{set a "[string range $a 0 [expr $j-1]]&lt;[string range $a [expr $j+1] end]"}
			if {[string index $a $j] == ">"}\
				{set a "[string range $a 0 [expr $j-1]]&gt;[string range $a [expr $j+1] end]"}
		    }
		}
		incr i
	    } {
		regsub -all {<} $a {\&lt;} a
		regsub -all {>} $a {\&gt;} a
	    }
	    if [regexp {^     CHAPTER ([0-9]+) +([A-Z_0-9\-]+) +[0-9]+ *}\
		    $a dummy num tag] {
		puts $fc "<B>CHAPTER $num  <A HREF=\"$num.html\" TARGET=\"Results\">$tag</A></B>"
		continue
	    }
	    if [regexp {^     Index +[0-9]+} $a] {
		puts $fc "\n<A HREF=\"Index.html\" TARGET=\"Results\">Index</A>"
		continue
	    }
	    if [regexp\
		    {^             ([0-9.]+) +([a-zA-Z0-9_/,\.\|\-]+) +[0-9]+ *}\
		    $a dummy tag title] {
		puts -nonewline $fc "    $tag  <A HREF=\"$tag.html\" TARGET=\"Results\">"
		if [regexp {[0-9]+\.[0-9]+\.[0-9.]+} $tag]\
			{puts $fc "<I>$title</I></A>"}\
			{puts $fc "$title</A>"}
		continue
	    }
	    if [regexp {^     ([1-9]+[0-9]*\.[1-9]+[0-9.]*) +([a-zA-Z0-9_/,\.\|\-]+)( *| .+)$} $a dummy tag title cmt] {
		set root [file rootname  $tag]
		set ext  [file extension $tag]
		Tail_Header $tag $title $cmt
		continue
	    }
	    if {$Index} {
		#regsub -all { ([0-9]+)(,|$)} $a { <A HREF="#\1">\1</A>\2} a
		regsub -all { ([0-9]+)(,|$)} $a {} a
	    }
	    if {$i == $i0} {
		if [regexp {^ +(Page|PAGE) +([0-9]+)}\
			$line(65) dummy dummy2 tag] {
		    set a "     <A NAME=\"$tag\">[string range [string trimright $a] 5 end] </A>"
		}
	    }
	    puts $fo [string range [string trimright $a] 5 end]
	}
    } {
	puts $fo "</PRE>\n</BODY>"
	close $fo
	close $fi
	puts $fc "</PRE>\n</BODY>"
	close $fc
	exec sed -e {s/ TARGET="Results"//g}\
		$DIR/contents.html > $DIR/old_index.html
	exec sed -e "s,\\(\[a-z_\]\\+\\).html,$DIR/\\1.html,g"\
		$DIR/index.html > topdrawer.html
	puts "\nEnd of Translation."
	exit
    }
}
