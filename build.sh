#!/usr/bin/env sh
git submodule init
git submodule update
cd tools ; make ; cd ..
cd src ; ./build.sh ; cd .. 
cd lib ; ./build.sh ; cd .. 
