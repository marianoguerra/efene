#!/usr/bin/env sh
../bin/fnc -o ../ebin spec.ifn type.ifn
../bin/fnc -o ../ebin rec.ifn
../bin/fnc -o ../ebin *.ifn

cd thirdparty/
cd ibrowse/
make
cp ebin/*.beam ../../../ebin/

cd ../mochiweb
make
cp ebin/*.beam ../../../ebin/

cd ../simple_bridge
make
cp ebin/*.beam ../../../ebin/

cd ../..
