#!/usr/bin/env sh
../bin/fnc rec.ifn spec.ifn type.ifn
mv *.beam ../ebin
../bin/fnc *.ifn
mv *.beam ../ebin
