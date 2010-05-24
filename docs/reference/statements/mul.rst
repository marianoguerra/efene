Mul Expression
--------------

::
 
        Expr1 op Expr2

Mul expressions are a group of expressions that have higher precedence than add
expressions.

The operators are:

====     =======================
op       Description
====     =======================
`*`      multiplication
`/`      division
`%`      integer reminder of X/Y
&        binary and
andd     boolean and
====     =======================

Examples::

        >>> 2 * 3
        6
        >>> 10 * 2
        20
        >>> 10 / 2
        5.0
        >>> 13 % 2
        1
        >>> 32 & 2
        0

