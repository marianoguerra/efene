Build efene on *nix
-------------------

To build efene on unix-like systems (Linux, BSDs, Mac OSX etc.) you need erlang
installed.

The installation depends on the operating system.

Download latest efene snapshot
::::::::::::::::::::::::::::::

Download the latest `version of efene`__

__ http://github.com/marianoguerra/efene/zipball/master

Unpack efene

Checkout the latest efene snapshot
::::::::::::::::::::::::::::::::::

You can get the latest version from git if you want to stay up to date,
install git and run::

        git clone http://github.com/marianoguerra/efene.git

Building efene
::::::::::::::

cd to efene/src

run::

        ./build.sh

Done!

To test it, do::

        cd ../bin
        fnc -s

You should see the erlang shell, type::

        >>> halt()

And press enter to quit.

Building fnc
::::::::::::

To build fnc you need gcc and make installed, this depends on the operating
system and/or distribution you are using.

Once you have gcc and make installed, cd to efene/tools and type::

        make

It should build fnc and move it to *bin/*
