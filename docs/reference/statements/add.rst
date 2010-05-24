Add Expression
--------------

::
 
        Expr1 op Expr2

Add expressions are a group of expressions that have lower precedence than mul
expressions.

The operators are:

====     ===================
op       Description
====     ===================
`+`      addition
`-`      substraction
`|`      binary or
^        binary xor
orr      boolean or
<<       binary shift left
>>       binary shift right
====     ===================

Examples::

        >>> 1 + 2
        3
        >>> 1 << 4
        16
        >>> 16 >> 4
        1
        >>> 16 | 1
        17
        >>> 16 ^ 1
        17
        >>> 1 + 2 - 2 | 1
        1

