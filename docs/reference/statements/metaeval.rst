Meta Eval
---------
.. _metaeval:

Top Level Meta Eval
:::::::::::::::::::

Used at module level, like all the :ref:`top level expressions <toplevelexpressions>`

::

        !$(Expr)

Meta Eval at Function Level
:::::::::::::::::::::::::::

Used inside function declarations.

::

        $(Expr)

.. waring::

        This feature is only needed for advanced users, you don't need to use
        it or understand it to code in efene

Eval an efene expression at *compile time* and insert the returned value into
the Abstract Syntax Tree (AST). 

The Abstract Syntax Tree is a tree of tuples that represent a program in a way
that is easy to manipulate by the compiler.

This tree can be modified by the usage of the *$(...)* statement to insert
aditional AST nodes before compiling a module.

This can be used to add new features to the language withouth modifying the
language syntax.

It's also useful to capture boilerplate code and move it to a library that
generates that code for us.

Examples
::::::::

::

        >>> $((integer, $line, 4))
        4
        
This expression writes the AST node to represent an integer of value 4 in line $line (the current line)

::

        >>> $((op, $line, '+', (integer, $line, 2), (integer, $line, 3)))
        5

This expressions writes the AST to represent the "2 + 3" expression.

Using it like this isn't really helpful, but we can move the generation to functions and reuse them.

Also, meta eval is really poweful coupled with the :ref:`astify statement <astify>`
