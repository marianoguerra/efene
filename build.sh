#!/usr/bin/env sh
export FNPATH=$(pwd)
cd tools ; make ; cd ..
./rebar compile
