Introduction
------------

Efene
~~~~~

efene is a programming language that runs on the erlang virtual machine.

the idea is to provide an alternative syntax to erlang that is most suitable
for people coming from languages like Java, C, C++, C#, Javascript.

the language is almost 100% compatible with erlang (and will be), the compiler
allows to translate an efene source file into a readable erlang one or compile
it directly to bytecode. It also adds some syntactic sugar in some places to
make some tasks easier.


Ifene
~~~~~

ifene is a dialect of efene that has all the same features and syntax from 
efene but differs in the way that blocks of code are delimited.

In efene blocks are delimited using curly brackets, in ifene blocks are
delimited using indentation like python.

Both languages use the same code to compile, the only difference between
them is that ifene is preprocessed to convert the indentation into curly
brackets before compiling it.


