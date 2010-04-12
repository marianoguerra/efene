#!/usr/bin/env sh
CWD=$(pwd);

for file in $(ls *.fn); do ../bin/fnc -o $CWD $file; done
for file in $(ls rosetta/*.fn); do ../bin/fnc -o $CWD $file; done
