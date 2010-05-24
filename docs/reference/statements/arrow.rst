Arrow Expression
----------------

::

        Expr->funcall([args])[->funcall([args])..]

An arrow expression allows to pass the result of one expression as first
argument of the function call after the arrow operator.

This operator exists to avoid creating multiple temporary variables when
doing multiple transformations on an expression.

The expression::

        >>> L = lists.seq(1, 5)
        [1,2,3,4,5]
        >>> L1 = lists.reverse(L)
        [5,4,3,2,1]
        >>> L2 = lists.append(L1, [9,8,7])
        [5,4,3,2,1,9,8,7]
        >>> Last = lists.last(L2)
        7

Can be expressed using the arrow expression as follows::

        >>> lists.seq(1, 5)->lists.reverse()->lists.append([9,8,7])->lists.last()
        7

Since the Expression before the arrow must be the first parameter in the
function call after the arrow, a set of modules are provided by efene to
operate on basic data types that receive the data type as first parameter on
all of its functions.

In source code a new line can be inserted after the arrow expression to make
the expression easier to read::

        lists.seq(1, 5)->
                lists.reverse()->
                lists.append([9,8,7])->
                lists.last()
