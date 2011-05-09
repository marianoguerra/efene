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

we declare with the public function attribute that the function is public, that
means that the function can be accessed from outside the module

.. literalinclude:: code/hello.fn
   :language: efene
   :lines: 2

we create a function called *run* that receives no arguments

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

let's compile the module we just created, we save the content on a file called
hello.fn and run::

        fnc hello.fn

here we compile the file hello.fn, if all goes well a file called hello.beam
with the compiled bytecode is created

to run the compiled program we run::

        fnc -r hello run

the -r switch instructs the *fnc* command to run the function called *run* from
the module called *hello*

we may want to play with our module from the shell, let's do that by running
the efene shell::

        fnc -s

now we call the function from the shell::

        >>> hello.run()
        Hello World!
        ok
        >>> 

The first line is the message printed to the standard output by the function,
the second line (*ok*) is the value returned by the function, this is the value
returned by the last statement on the function body, in this case, the value
returned by io.format.

to exit press Ctrl+D

hello world take 2
::::::::::::::::::

let's make the example a little more complex now, we could create a function
that says hello to anyone, not just world:

.. literalinclude:: code/hello1.fn
   :language: efene
   :linenos:

here we defined two public functions, one called *hello* that receives one
parameter called *Name*

.. literalinclude:: code/hello1.fn
   :language: efene
   :lines: 3

that when called will print the string resulting from the replacement of the
string (~s) contained in the *Name* parameter

.. literalinclude:: code/hello1.fn
   :language: efene
   :lines: 4

then we change the body of the *run* function to call the *hello* function
twice with two different parameters

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

since the efene shell doesn't have readline support builtin you can have it by
using *rlwrap* that is a command that comes with the *readline* package

running the shell like this::

        rlwrap fnc -s

gives you all the readline features until they are supported by the shell

hello world take 3
::::::::::::::::::

nice, we have a more generic hello function, but since I mostly salute to the
world I want my old version back too, let's do that:

.. literalinclude:: code/hello2.fn
   :language: efene
   :linenos:

in this version we created a new function called *hello* that takes no
parameters and when called it will call the version of the function that
receives one parameter passing "World", in this way we have our old version
back and we are reusing the code we already wrote.

as you can see, we can have multiple functions with the same name as long as
they have a different arity. In this case we have *hello/0* and *hello/1*.

let's define arity
..................

let's admit it, I'm lazy, and writing "the function that receives one
parameter" is to long for me, luckily there is a word to avoid typing, this
word is called *arity*.

*arity* means the number of parameters that a function receives, so "a function
that receives no parameters" is a function of arity 0, " a function that
receives 1 parameter" is a function of arity 1 and so on.

but I still have to type "the hello function of arity 1"!, another expression
to the rescue. When we want to refer to a function with a given name and a
given arity we can use *functionname/arity*, for example: "the hello function
that takes 1 parameter" is *hello/1*.

from now on I will use this way to refer to functions.

all animals are equal, but some animals are more equal than others
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

our hello world program is growing, and now we need to do a special case for
our hello function, let's say that we have to salute Sir Winston Churchill, we
should give him a better message than "hello Winston Churchill!"

for that we will add a function clause to our *hello/1* function to pattern
match for the specific case and do something different:

.. literalinclude:: code/hello3.fn
   :language: efene
   :linenos:

function clause? pattern matching? I knew you would start talking weird to me
.............................................................................

to avoid typing a lot and to use the same vocabulary when talking about
programming we need some ground definitions, in this case I needed to use two
new concepts:

a function clause is a case of a function for a given set of parameters that
will excecute a given body, until now we used functions with just one function
clause that stored the given parameters on some variables.

Those parameters could contain any value. But we can do more than that, we can
specify that a given body will only be excecuted when the parameters *match*
some values, this is called pattern matching.

.. literalinclude:: code/hello3.fn
   :language: efene
   :lines: 2-10

in this case, the first function clause of the function *hello/1* will pattern match
the argument to the string "Winston Churchill", if that matches it will excecute:

.. literalinclude:: code/hello3.fn
   :language: efene
   :lines: 4

otherwise it will try to match the next function clause, in this case the next
function clause will store the content of the argument on the variable *Name*
so it will always match and the body will be executed.

got no match
............

but what happens if no function clause matches? well let's try it and find it
for ourselves::

        >>> OnlyMatchOne = fn (1) { one }
        #Fun<erl_eval.6.13229925>
        >>> OnlyMatchOne(1)
        one
        >>> OnlyMatchOne(2)
        exception throw: function_clause
        >>> 

here we defined a function called *OnlyMatchOne* that has one function clause
that will match the number one and return the atom *one*

then we called the function with the argument 1 and it worked as expected.

then we called the function with the argument 2 and it threw an exception because
there is no function clause that matched that value.

this is useful as a kind of assertion of the values the function accepts, we
match only the values we know how to handle and let the runtime raise an
exception if another value is provided.

this is an example of the "let it crash" zen of erlang.

other ways to do the same
:::::::::::::::::::::::::

we can produce the same result as the last example using other features of the
language, guards, switch/case and if statements

guards
......

guards are a way to run a given function clause only when the conditions after
the *when* are true.

with pattern matching on the function arguments we only match for equality of
values, on guards we can test for other things, like the length of a string, if
some values have some relations (!=, >, <, >=, <= etc.)

.. literalinclude:: code/hello4.fn
   :language: efene
   :lines: 2-9

if statement
............

the if statement is a control structure of the language that allows to execute
the body only if the condition is true, it works similarly to guards but can be
used inside function bodies.

.. literalinclude:: code/hello5.fn
   :language: efene
   :lines: 2-10

switch/case statement
.....................

the switch/case statement is a control structure of the language that allows to execute
the body only if the expression in switch matches the expression in case.

this is useful if we have to compare a variable or expression agains several values and
do something different on each one.

.. literalinclude:: code/hello6.fn
   :language: efene
   :lines: 2-12

everything is an expression
:::::::::::::::::::::::::::

in efene everything returns a value, if you see the previous examples you can see we use *io.format*
all over the place, but what if we wanted to put the result of the function on a file, display it on
a web server or send it to another process?

then we will have to make the *hello* function return the value and use it in other functions, let's redo
our previous examples taking advantage of "everything is an expression".

more pattern matching
:::::::::::::::::::::

we saw some basic pattern matching done on the function arguments, but pattern
matching is available on more places, here I will show some things we can do
with pattern matching on the command line::

 dev $ rlwrap fnc -s
 >>> A = 4
 4
 >>> B = 4
 4
 >>> C = 5
 5
 >>> # this is not an assignment but pattern matching
 >>> A = B
 4
 >>> # both patterns match, so it's ok
 >>> # now we will try to match two different values
 >>> A = C
 exception throw: {badmatch,5}
 >>> # it throws a badmatch exception, that means that both values didn't match
 >>> # let's use pattern matching for something more advanced
 >>> L = [1, 2, 3, 4]
 [1, 2, 3, 4]
 >>> # we will match, some of the values to literals, some of them put them on variables and discard others
 >>> # First and Second are variable names, so whatever value was in that place it will be assigned to that
 >>> # variable
 >>> # the 2 in the second position is a literal, so it will have to match to the value on the second position
 >>> # of L
 >>> # the variable with the name _ means "ignore this value I won't use it"
 >>> [First, 2, Second, _] = L
 [1, 2, 3, 4]
 >>> # we can see the assigned values
 >>> First
 1
 >>> Second
 3
 >>> # we can do the same with tuples
 >>> T = (1, 2, (3, 4), 5)
 (1, 2, (3, 4), 5)
 >>> # even with tuples inside tuples
 >>> (First, _, (FirstNested, SomeName), 5) = T
 (1, 2, (3, 4), 5)
 >>> First
 1
 >>> FirstNested
 3
 >>> SomeName
 4
 >>> # let's define a new list with a value changed
 >>> L1 = [1, 42, 3, 4]
 >>> # try to match against the same pattern as before
 >>> [First, 2, Second, _] = L1
 exception throw: {badmatch,[1,42,3,4]}
 >>> # since two isn't equal to 42 we get a badmatch exception
 >>> # but something special happened above, since First and Second were
 >>> # already bound to values, we were pattern matching against those values
 >>> # instead of assigning them to the values in the list.
 >>> # since the values were the same there was no problem, but let's try with a different value
 >>>
 >>> # let's bind the name NewFirst to the value 14
 >>> NewFirst = 14
 14
 >>> # and try to match against the list L1
 >>> [NewFirst, 2, Second, _] = L1
 exception throw: {badmatch,[1,42,3,4]}
 >>> # this failed because NewFirst (14) isn't equal to 1
 >>> # let's make it match
 >>> [NewFirst, 2, Second, _] = [14, 2, 3, 999]
 [14, 2, 3, 999]

data types
::::::::::

on our examples we have seen different data types, mainly integers, strings,
lists and tuples, let's define them here so you know them all.

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
contains other characters than alphanumeric characters, underscore (_), or @::

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

structs
.......

structs are a way to represent something similar to objects, they have the same
syntax as JSON and object literals in javascript::

        >>> P1 = {firstname: "Mariano", lastname: "Guerra"}
        {firstname: "Mariano", lastname: "Guerra"}
        >>> Name = fn (Self) { Self.firstname ++ " " ++ Self.lastname }
        #Fun<erl_eval.6.13229925>
        >>> Name(P1)
        "Mariano Guerra"
        >>> P2 = {firstname: "Luis Mariano", lastname: "Guerra", name: Name}
        {firstname: "Luis Mariano", lastname: "Guerra"}
        >>> P2.name()
        "Luis Mariano Guerra"
        >>> 

as you can see, we can add functions to our structs, functions in structs work
like methods in python, they explicitly receive the object they apply to as
first argument, by convention this argument is called *Self*.

when you call a struct's method the struct is implicitly passed as first
parameter.


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

