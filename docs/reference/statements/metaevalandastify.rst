Meta Eval and Astify Expression
-------------------------------
.. _metaevalandastify:

::

        $[Expr]

 
.. warning::

        This feature is only needed for advanced users, you don't need to use
        it or understand it to code in efene

Evals an expression at compile time and astifies the result, this expression is a shortcut to::

        [|$(Expr)|]

This is useful to generate time consuming things one time at compile time without
the need to hardcode them.

We can use functions that return erlang/efene data structures and convert the
result to AST.

Examples
::::::::

We want to precompute something at compile time and put the result to avoid
generating the value at runtime each time the function is called.

A simple example::

        longtuple = fn ()
            $[list_to_tuple(lists.seq(1, 10))]

Compiling to erlang to see the result::

        longtuple() -> {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}.


