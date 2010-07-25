#!/usr/bin/env sh
../bin/fnc spec.ifn type.ifn
mv *.beam ../ebin
../bin/fnc rec.ifn
mv *.beam ../ebin
../bin/fnc *.ifn
mv *.beam ../ebin
