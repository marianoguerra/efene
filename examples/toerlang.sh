#!/usr/bin/env sh

for file in $(ls *.fn)
do 
	echo
	echo "$file in erlang:"; 
	../bin/fn2erl $file;
done
