CC=gcc 
CFLAGS=-Wall
main: fnc.o fnc
		mv fnc ../bin/fncng

clean:
		rm -f fnc.o ../bin/fncng

lint:
		splint fnc.c

check:
		valgrind fnc


