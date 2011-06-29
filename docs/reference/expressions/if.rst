.. _ifexpression:

If Expression
-------------

.. code-block:: efene

        # simple form
        if (Expr1) {
            Body1
        }


.. code-block:: efene

        # multiple ifs
        if (Expr1) {
            Body1
        }

        else if (Expr2) {
            Body2
        }

        else if (ExprN) {
            BodyN
        }


.. code-block:: efene

        # if/else
        if (Expr1) {
            Body1
        }

        else {
            ElseBody
        }


.. code-block:: efene

        # if/else if/else
        if (Expr1) {
            Body1
        }

        else if (ExprN) {
            BodyN
        }

        else {
            ElseBody
        }

The branches of an if-expression are scanned sequentially until a guard
sequence Expr which evaluates to true is found. Then the corresponding Body
(sequence of expressions) is evaluated.

The return value of Body is the return value of the if expression.

If no guard sequence is true, an run-time error will occur.

If necessary, the else branch can be used in the last branch, as that
expression is always true.


.. note::

        Parenthesis around conditions are optional

Example:

.. code-block:: efene

        is_greater_than = fn (X, Y) {
            if X > Y {
                true
            }

            else {
                false
            }
        }

Difference Between *if* and *when*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if expressions are another way of writting switch statements that avoid the nesting
of multiple switchs inside eachother, any boolean expression can be in the Expr
of an if expression.

when expressions are a way to write guards in the body of a function, a guard can
only contain a subset of expressions that are known to evaluate in constante time,
because of that no function calls can be done in the GuardSeq of a when expression.

when expressions are in efene only for compatibility with erlang, the if or switch
expressions are recommended.
