#!/usr/bin/env sh
../bin/fnc spec.ifn type.ifn -o ../ebin
../bin/fnc rec.ifn -o ../ebin
../bin/fnc *.ifn -o ../ebin
