#!/usr/bin/env sh
fnc -C "$1" | pygmentize -f html -l erlang 2>/dev/null
