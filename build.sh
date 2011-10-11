#!/usr/bin/env sh
export FNPATH=$(pwd)
cd tools ; make ; cd ..
cd src ; ./build.sh ; cd .. 
cd lib ; ./build.sh ; cd .. 
