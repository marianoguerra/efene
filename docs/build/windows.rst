Build efene on windows
----------------------

To run efene on windows you need erlang installed.

Go to the `erlang download page`__

__ http://erlang.org/download.html

Download the latest version from the row
*Windows binary (incl. documentation)*

Install it.

Download the latest `version of efene`__

__ http://github.com/marianoguerra/efene/zipball/master

Unpack efene

Adding efene and erlang to the PATH variable
::::::::::::::::::::::::::::::::::::::::::::

 #) right click on "My Computer" icon
 #) click properties
 #) go to the "Advanced" tab
 #) click the "Environment Variables"
 #) in the "System Variables" frame select the *Path* variable
 #) click the "Edit" button
 #) in the "Variable Value" text field go to the end of the text (don't edit it)

    1) add the path to the bin directory of your erlang instalation

       * separate the path from the previous one with a semicolon

    2) add the path to the bin directory of your efene instalation

       * separate the path from the previous one with a semicolon

 #) press the "Ok" button in the open windows until all are closed

In my case I added to the Path variable the following text::

        ;C:\Program Files\erl5.8\bin;C:\Documents and Settings\Mariano Guerra\My Documents\efene\bin

.. note:

        This values will change depending on the language of your installation,
        your username and the places where you installed erlang and efene.

Building efene
::::::::::::::

Open cmd.exe (Windows + r or Start -> run -> cmd.exe), cd to the src folder
inside efene, in my case it's::

        C:\Documents and Settings\Mariano Guerra\My Documents\efene\src

Type::

        build.bat

Done!

To test it, do::

        cd ../bin
        fnc.exe -s

You should see the erlang shell, type::

        >>> halt()

And press enter to quit.

Building fnc.exe
::::::::::::::::

.. note:

        you don't need to build fnc.exe since it's already available in *bin/*
        when you download efene.

        This step is only needed if you want to work on fnc.c

Install `MinGW`__

__ http://sourceforge.net/downloads/mingw/Automated%20MinGW%20Installer/MinGW%205.1.6/MinGW-5.1.6.exe/

Add the path to the bin directory inside MinGW to the Path environment variable.

Inside cmd.exe go to the tools directory inside efene and type::

        build.bat

It will compile fnc.c and move it to the *bin/* directory.
