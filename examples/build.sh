#!/usr/bin/env sh
CWD=$(pwd);

for file in $(ls *.fn); do ../bin/fnc $CWD $file; done
