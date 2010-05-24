If Expression
-------------

.. code-block:: efene

        # simple form
        if GuardSeq1 {
            Body1
        }


.. code-block:: efene

        # multiple ifs
        if GuardSeq1 {
            Body1
        }

        else if GuardSeq2 {
            Body2
        }

        else if GuardSeqN {
            BodyN
        }


.. code-block:: efene

        # if/else
        if GuardSeq1 {
            Body1
        }

        else {
            ElseBody
        }


.. code-block:: efene

        # if/else if/else
        if GuardSeq1 {
            Body1
        }

        else if GuardSeqN {
            BodyN
        }

        else {
            ElseBody
        }

The branches of an if-expression are scanned sequentially until a guard
sequence GuardSeq which evaluates to true is found. Then the corresponding Body
(sequence of expressions) is evaluated.

The return value of Body is the return value of the if expression.

If no guard sequence is true, an if_clause run-time error will occur.

If necessary, the else branch can be used in the last branch, as that guard
sequence is always true.

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
