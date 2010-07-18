Meta Eval Expression
--------------------
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

.. warning::

        This feature is only needed for advanced users, you don't need to use
        it or understand it to code in efene

Eval an efene expression at *compile time* and insert the returned value into
the Abstract Syntax Tree (AST). 

The Abstract Syntax Tree is a tree of tuples that represent a program in a way
that is easy to manipulate by the compiler.

This tree can be modified by the usage of the *$(...)* statement to insert
additional AST nodes before compiling a module.

This can be used to add new features to the language without modifying the
language syntax.

It's also useful to capture boilerplate code and move it to a library that
generates that code for us.

Special Case - Referring to Runtime Variables
:::::::::::::::::::::::::::::::::::::::::::::

If we want to refer to variables that will be available at runtime we can use the expression::

        $(VarName)

This will look at the variable at runtime and not at compile time.

This is useful when generating code that will access a value at runtime and allows to write 
*template ASTs* that are complete at runtime when the variable has a value.

See :ref:`astify statement <astify>` for examples.

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

Also, meta eval is really powerful coupled with the :ref:`astify statement <astify>`

