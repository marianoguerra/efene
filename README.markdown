# efene git

efene is a programming language that runs on the erlang virtual machine.

the idea is to provide an alternative syntax to erlang that is most suitable
for people coming from languages like Java, C, C++, C#, Javascript.

the language is almost 100% compatible with erlang (and will be), the compiler
allows to translate an efene source file into a readable erlang one or compile it directly to bytecode.
It also adds some syntactic sugar in some places to make some tasks easier.

to see how it looks you can go to the [examples dir](http://github.com/marianoguerra/efene/tree/master/examples/)

## Participate

a mailing list is available at [librelist](http://librelist.com) just send a mail to efene@librelist.com to subscribe.

as first mail you may send a hello world program in efene and present yourself by saying your name, where you are, how did you heard about efene and anything else you would like to say.

## Build instructions

### clone the repository with the latest code
git clone git://github.com/marianoguerra/efene.git

### go to the source directory
cd efene/src/

### build the binaries
./build.sh

### go to the examples folder
cd ../examples/

### build the examples
./build.sh

### run the examples
./run.sh

## Useful links

* [Website](http://marianoguerra.com.ar/efene)
* [Blog](http://efene.tumblr.com)
* [Central repo](http://github.com/marianoguerra/efene) (fork this one)
* [Tracker](http://github.com/marianoguerra/efene/issues)
* [Rosetta code page](http://rosettacode.org/wiki/Efene)
