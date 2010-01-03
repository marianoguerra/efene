#!/bin/sh

echo "building fnc"
8g fnc.go
8l -o fnc fnc.8 
rm fnc.8
mv fnc ../../bin

