#!/usr/bin/env sh
pushd tools ; make ; popd
pushd src ; ./build.sh ; popd
pushd lib ; ./build.sh ; popd
