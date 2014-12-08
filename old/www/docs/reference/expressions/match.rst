Match Expression
----------------

::

        Expr1 = Expr2

Matches Expr1, a pattern, against Expr2. If the matching succeeds, any unbound
variable in the pattern becomes bound and the value of Expr2 is returned.

If the matching fails, a badmatch run-time error will occur.

Examples::

        >>> (A, B) = (answer, 42)
        (answer,42)
        >>> A
        answer
        >>> (C, D) = [1, 2].
        ** exception error: no match of right hand side value [1,2]
