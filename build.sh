#!/usr/bin/env sh
cd tools ; make ; cd ..
cd src ; ./build.sh ; cd .. 
cd lib ; ./build.sh ; cd .. 
