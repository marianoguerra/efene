Function Expression
-------------------

Functions are the main building block in efene, that's why it's called
a `functional programming language`__.

__ http://en.wikipedia.org/wiki/Functional_programming

A function can be defined in the top level of a module or inside other
functions, both ways have the same syntax.

One example of a function that is called hello, receives no parameters
and returns the string *"hello world"*:

.. code-block:: efene

        hello = fn () {
                "hello world"
        }

The function declaration consists of an identifier for the function followed by
the equal sign, the *fn* keyword and zero or more parameters between
parenthesis, then the opening block symbol and the body of the function that
can consist of one or more statements separated by a new line followed by the
closing block symbol.

A function called sum that receives two parameters called *Num1* and *Num2* and
returns the sum of both numbers:


.. code-block:: efene

        sum = fn (Num1, Num2) {
                Num1 + Num2
        }

The result of the last expression is returned, no *return* keyword is needed.

An example of the same function but storing the result value in a new variable:

.. code-block:: efene

        sum = fn (Num1, Num2) {
                Result = Num1 + Num2
                Result
        }

Calling the function is done by specifying the name of the function followed by
the parameters between parenthesis separated by comas:

.. code-block:: efene
        
        sum(2, 5)

Function Name
~~~~~~~~~~~~~

A function declared at the top level of a module must have a name that starts
with lower case representing an atom.

A function declared inside another function must have a name that starts
with upper case representing a variable.

An example of a function defined inside another function:

.. code-block:: efene

        sum = fn (Num1, Num2) {
                AddTwoNumbers = fn (A, B) {
                        A + B
                }

                AddTwoNumbers(Num1, Num2)
        }

A function is a first class citizen in efene that means that a function can
be treated as any other type in a program.

Multiple Definitions for a Function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A function can have multiple implementations with the same name as long as the
different implementations receive different number of parameters, a function
is defined by its name and its arity. 

The arity of a function is the number of parameters it receives, so two functions
with the same name but different arity are considered different functions.

An example of two definitions for sum:

.. code-block:: efene

        sum = fn (Num1, Num2) {
                Num1 + Num2
        }

        sum = fn (Num1, Num2, Num3) {
                Num1 + Num2 + Num3
        }

Pattern Matching in a Function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A function can contain multiple bodies that will be executed when the pattern
defined in it's parameters match, when a function is called the execution starts
in the first definition, if it doesn't match then the next definition is tried.

If no definition match an exception is thrown.

An example of pattern matching in a function that calculates the division between
two numbers is show, in this case the case in which the denominator is 0 has a
special body that returns the atom *error* instead of the division itself:

.. code-block:: efene

        div = fn (Num1, 0) {
                error
        }

        fn (Num1, Num2) {
                Num1 / Num2
        }

Since the matching is done from top to bottom the most generic definitions must
be at the bottom to avoid matching before a more specific one.

The format of a definition with multiple is the same as a simple function
definition but continues with the *fn* keyword and as another definition.

Guards
~~~~~~

A function can contain a guard, that is a condition that must be evaluated to
true in order to execute that body.

The previous function expressed using guards would be:

.. code-block:: efene

        div = fn (Num1, Num2) when Num2 == 0 {
                error
        }

        fn (Num1, Num2) {
                Num1 / Num2
        }

A guard consists of the word *when* followed by an boolean expression, if the
expression evaluates to true then the body is executed, otherwise the next
definition is evaluated.

Visibility of a Function Outside its Module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A function declared in a module is by default only visible inside the module it
was defined.

To make a function visible outside the module where it was defined the attribute
*@public* must be added before the function definition.

An example:

.. code-block:: efene

        @public
        sum = fn (Num1, Num2) {
                Result = Num1 + Num2
                Result
        }



