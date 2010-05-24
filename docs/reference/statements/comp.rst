Comp Expression
---------------

Returns the Boolean value of the expression, true or false.

===     =========================
op      Description
===     =========================
==      equal to
!=      not equal to
<=      less than or equal to
<       less than
>=      greater than or equal to
>       greater than
===     exactly equal to
!==     exactly not equal to
===     =========================

The arguments may be of different data types. The following order is defined::

        number < atom < reference < fun < port < pid < tuple < list < bit string

Lists are compared element by element. Tuples are ordered by size, two tuples
with the same size are compared element by element.

If one of the compared terms is an integer and the other a float, the integer
is first converted into a float, unless the operator is one of === and !==.

If the integer is too big to fit in a float no conversion is done, but the
order is determined by inspecting the sign of the numbers.

Examples::

        >>> 1 == 1.0
        true
        >>> 1 !== 1.0
        false
        >>> 1 > a
        false

