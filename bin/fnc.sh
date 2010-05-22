#!/usr/bin/env sh
SCRIPTDIR=$(dirname $0)
erl -run fn run "$@" -run init stop -noshell -pa $SCRIPTDIR/../ebin/
