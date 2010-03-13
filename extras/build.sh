#!/usr/bin/env sh
NAME="../docs/tutorial.html"
./preamble.sh > $NAME
./body.sh >> $NAME
./end.sh >> $NAME
