set f [open |td w]
puts $f "0 0 ; 1 1 ; join 1 ; flush ; list ; show cursor \n"
close $f
