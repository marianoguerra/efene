#!/usr/bin/env sh
git submodule init
git submodule update
export FNPATH=$(pwd)
cd tools ; make ; cd ..
cd src ; ./build.sh ; cd .. 
cd lib ; ./build.sh ; cd .. 
