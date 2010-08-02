mingw32-gcc.exe -c -o fnc.o fnc.c
mingw32-gcc.exe fnc.o -o fnc
move fnc.exe ../bin
del fnc.o
