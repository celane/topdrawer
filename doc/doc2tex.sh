#!/bin/sh
sed -e 's/\[[0-9]m//g' < topdrawer4.0.hlp | doc2tex | \
sed -e '/\\end{document}/i\
\\end{verbatim}' > topdrawer.tex
