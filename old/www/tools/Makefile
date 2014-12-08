CC=gcc 
CFLAGS=-Wall
main: fnc.o fnc
		mkdir -p ../bin
		mv fnc ../bin/fnc

clean:
		rm -f fnc.o

lint:
		splint fnc.c

check:
		valgrind fnc
