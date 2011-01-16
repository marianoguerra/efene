efene quickstart
----------------

welcome to efene quickstart, this document asumes you know something about programming and you want to know what is efene about.

the basics
::::::::::

efene is a functional programming language for the erlang platform with a syntax that is a mix of python and javascript.

efene comes in two flavors:

efene
        defines blocks using curly brackets like C/C++/Java/Javasript/C#, identified by the file extension *.fn*

ifene
        defines blocks using indentation like python, identified by the file extension *.ifn*

you can use the one you like the most, the only changes between both is the way to define blocks, the rest is all the same.

efene could be said to be a way to write erlang with different syntax, it also adds some features above that.

efene is 100% compatible with erlang, that means:

 * efene modules can be used from erlang
 * erlang modules can be used from efene
 * efene has access to OTP
 * efene can be translated to readable erlang

hello world
:::::::::::

hello worlds look simple and awesome on all programming languages, but it's a good place to start:

.. literalinclude:: code/hello.fn
   :language: efene
   :linenos:

we declare with the public function attribute that the function is public, that means that the function can be accessed from outside the module

.. literalinclude:: code/hello.fn
   :language: efene
   :lines: 2

we created a function called *run* that receives no arguments

.. literalinclude:: code/hello.fn
   :language: efene
   :lines: 3

that has a body that calls the function *format* from the module *io* and
passes one string argument containing the message to display

.. literalinclude:: code/hello.fn
   :language: efene
   :lines: 4

the *~n* at the end of the string means "print a new line"

compiling and running
.....................

let's compile the module we just created, we save the content on a file called hello.fn and run::

        fnc hello.fn

here we compile the file hello.fn, if all goes well a file called hello.beam with the compiled bytecode is created

to run the compiled program we run::

        fnc -r hello run

we may want to play with our module from the shell, let's do that by running the efene shell::

        fnc -s

now we call the function from the shell::

        >>> hello.run()
        Hello World!
        ok
        >>> 

and press Ctrl+D to exit

hello world take 2
::::::::::::::::::

let's make the example a little more complex now, we could create a function that says hello to anyone, not just world:

.. literalinclude:: code/hello1.fn
   :language: efene
   :linenos:

here we defined two public functions, one called *hello* that receives one parameter called *Name*

.. literalinclude:: code/hello1.fn
   :language: efene
   :lines: 3

that when called will print the string resulting from the replacement of the string (~s) contained in the *Name* parameter

.. literalinclude:: code/hello1.fn
   :language: efene
   :lines: 4

then we change the body of the *run* function to call the *hello* function twice with two different parameters

.. literalinclude:: code/hello1.fn
   :language: efene
   :lines: 9,10

compile::

        fnc hello1.fn

run::

        fnc -r hello1 run

play::

        $ fnc -s
        >>> hello1.run()
        Hello World!
        Hello coders!
        ok
        >>> hello1.hello("me")
        Hello me!
        ok
        >>> 

usage hint
::::::::::

since the efene shell doesn't have readline support builtin you can have it by using *rlwrap* that is a command that comes with the *readline* package

running the shell like this::

        rlwrap fnc -s

gives you all the readline features until they are supported by the shell

data types
::::::::::

numbers
.......

numbers in efene can be integers or floats::

        >>> 42
        42
        >>> 432423423543654563437643754634363464356345
        432423423543654563437643754634363464356345
        >>> 2.3
        2.3

boolean
.......

boolean data type::

        >>> true
        true
        >>> false
        false
        >>> not true
        false
        >>> not false
        true


atoms
.....

An atom is a literal, a constant with name. An atom should be enclosed in
single quotes (') if it does not begin with a lower-case letter or if it
contains other characters than alphanumeric characters, underscore (_), or @.

        >>> ok
        ok
        >>> error
        error
        >>> 'ERROR'
        'ERROR'

lists
.....

like a list in any other language, can contain any type inside it, even nested lists::

        >>> []
        []
        >>> [1]
        [1]
        >>> [1, 2.3, false, ok, "hi"]
        [1, 2.3, false, ok, [104, 105]]
        >>> [[1, 2], [3, 4]]
        [[1, 2], [3, 4]]

strings
.......

as seen above strings are just lists of integers in efene::

        >>> "hi"
        [104, 105]
        >>> [$h, $i]
        [104, 105]
        >>> [104, 105]
        [104, 105]

the *$* operator allows to get the numeric value of a character

tuples
......

a tuple is an ordered set of elements with fixed size::

        >>> (,)
        (,)
        >>> (1,)
        (1,)
        >>> (1, two, 3.2, true)
        (1, two, 3.2, true)

as you can see the empty tuple is expressed as "(,)" and the one item tuple
ends with a comma to differentiate it from an expression between parenthesis

basic language constructs
:::::::::::::::::::::::::

here are some examples of basic language constructs

if/else if/else
...............

.. literalinclude:: code/basics.fn
   :language: efene
   :lines: 4-12

switch/case/else
................

.. literalinclude:: code/basics.fn
   :language: efene
   :lines: 17-29

for/in
......

.. literalinclude:: code/basics.fn
   :language: efene
   :lines: 34-36

complete program to run
.......................

.. literalinclude:: code/basics.fn
   :language: efene

output::

        < 0
        >= 0 and < 10
        >= 10
        one!
        not one nor two
        "spam"
        "eggs"
        "bacon"


everything is an expression
:::::::::::::::::::::::::::

in efene everything is an expression, that means that everything returns something and can be assigned to a variable

here are some examples from that

.. literalinclude:: code/basics1.fn
   :language: efene

output::

        item 1
        item 2
        item 3
        item 4
        item 5
        item 6
        item 7
        item 8
        item 9
        item 10
        items [1,2,3,4,5,6,7,8,9,10]
        items plus one [2,3,4,5,6,7,8,9,10,11]
        age: -2 = error
        age: 15 = minor
        age: 25 = adult
        Place 1 = Champion
        Place 5 = Work hard next time!

