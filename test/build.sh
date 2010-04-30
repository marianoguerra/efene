#!/usr/bin/env sh
for i in *.erl; do echo $i; erlc $i; done
