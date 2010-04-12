#!/usr/bin/env sh

for file in $(ls *.beam)
do 
	name=$(basename $file .beam);
	echo
	echo "running $name"; 
	../bin/fn $name run;
done

