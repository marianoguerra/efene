#!/usr/bin/env sh
erlc build.erl
erl -run build build -run init stop | grep -v "{yeccpars" | grep -v "{yy_"
