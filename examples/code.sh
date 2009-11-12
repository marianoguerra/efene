#!/usr/bin/env sh
echo "<html><head><style>" > code.html
 
pygmentize -f html -S colorful >> code.html
 
echo "</style></head><body>" >> code.html
 
for file in $(ls *.fn); do echo "<h2>$file</h2>" >> code.html; pygmentize -f html $file >> code.html; done
 
echo "</body></html>" >> code.html

