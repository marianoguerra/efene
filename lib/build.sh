#!/usr/bin/env sh
fnc spec.ifn type.ifn -o ../ebin
fnc rec.ifn -o ../ebin
fnc *.ifn -o ../ebin
