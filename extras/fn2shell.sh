#!/usr/bin/env sh
fnc -c "$1" | pygmentize -f html -l efene 2>/dev/null
