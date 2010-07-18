Astify Expression
-----------------
.. _astify:

::

        [|Expr|]

.. warning::

        This feature is only needed for advanced users, you don't need to use
        it or understand it to code in efene

Astify returns the Abstract Syntax Tree (AST) representation of the expression it encloses.

It's used to generate code that can be inserted at compile time to make meta programming easier.


Examples:
:::::::::

::

        >>> [|42|]
        {integer,1,42}

        >>> [|1 + 2|]
        {op,1,'+',{integer,1,1},{integer,1,2}}

This statement gets powerful when used with :ref:`meta eval <metaeval>`, this allows to
reference variables at runtime inside ASTs generated at compile time.

With this we can generate a template AST that will be filled at runtime with the rest of
the AST when the user completes the template with the part that is missing.

::

        >>> AddOne = fn (Ast) { [| 1 + $(Ast) |] }
        #Fun<erl_eval.6.13229925>

        >>> AddOne([|2|])
        {op,1,'+',{integer,1,1},{integer,1,2}}

        >>> [|A + B|]
        {op,1,'+',{var,1,'A'},{var,1,'B'}}

        >>> SomeVar = 3
        3

        >>> [|$(SomeVar)|]
        3

In this case we create a function that generates an AST for an expression that adds 1 to something.

That something will be filled with the parameter that the *AddOne* function receives, so we can pass
in the AST for an integer, a float, a function call or anything we want, the returned value is the
AST to do that::

        >>> AddOne([|1.2|])
        {op,1,'+',{integer,1,1},{float,1,1.2}}
        >>> AddOne([|abs(-1)|])
        {op,1,'+',{integer,1,1},{call,1,{atom,1,abs},[{op,1,'-',{integer,1,1}}]}}



