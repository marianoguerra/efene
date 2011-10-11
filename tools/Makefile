CC=gcc 
CFLAGS=-Wall
main: fnc.o fnc
		mv fnc ../bin/fnc

clean:
		rm -f fnc.o

lint:
		splint fnc.c

check:
		valgrind fnc
