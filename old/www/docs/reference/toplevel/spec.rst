.. _specexpression:
Spec Definition
---------------

The spec expression is used to define the argument and return types for a function.

It uses the same syntax as the *@type* expression.

For more information see the `Erlang Enhancement Proposal 8`__.

__ http://www.erlang.org/eeps/eep-0008.html

Syntax
======

The basic format is as follows::

        @spec(<ArgType1>, ..., <ArgTypeN>) -> <ReturnType>

Also, for documentation purposes, argument names can be given::

        @spec(<ArgName1> :: <Type1>, ..., <ArgNameN> :: <TypeN>) -> <ReturnType>

Type variables can be used in specifications to specify relations for the input
and output arguments of a function. For example, the following specification
defines the type of a polymorphic identity function::

        @spec(X) -> X

Examples
========

A complete function definition with its spec::

        @public
        @spec(VarName :: foo() | bar()) -> foo() | bar()
        f_2 = fn (Val)
            Val

Some spec definitions::

        @spec(list(integer() | float())) -> list(integer() | float())
        @spec(Foo :: [char()]) -> list(char())
        @spec((atom(),atom(),byte())) -> (atom(),atom(),byte())

