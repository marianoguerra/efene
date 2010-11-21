.. _blockstatement:

Block Expression
----------------

.. code-block:: efene

        begin {
                Expr1
                Expr2

                ExprN
        }

Block expressions provide a way to group a sequence of expressions, similar to
a clause body. The return value is the value of the last expression ExprN.
