#!/usr/bin/env sh
echo "rebuilding tests"
./build.sh
echo "running tests"
erl -run tests all -run init stop -noshell -pa ../ebin
