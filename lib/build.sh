#!/usr/bin/env sh
../bin/fnc spec.ifn type.ifn -o ../ebin
../bin/fnc rec.ifn -o ../ebin
../bin/fnc *.ifn -o ../ebin

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
