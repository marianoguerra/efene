#!/usr/bin/env sh
erl -run efene run $@ -run init stop -noshell -pa ebin
