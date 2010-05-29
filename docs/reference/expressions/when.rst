When Expression
---------------

.. code-block:: efene

        # simple form
        when (GuardSeq1) {
            Body1
        }


.. code-block:: efene

        # multiple whens
        when (GuardSeq1) {
            Body1
        }

        else when (GuardSeq2) {
            Body2
        }

        else when (GuardSeqN) {
            BodyN
        }


.. code-block:: efene

        # when/else
        when (GuardSeq1) {
            Body1
        }

        else {
            ElseBody
        }


.. code-block:: efene

        # when/else when/else
        when (GuardSeq1) {
            Body1
        }

        else when (GuardSeqN) {
            BodyN
        }

        else {
            ElseBody
        }

The branches of an when-expression are scanned sequentially until a guard
sequence GuardSeq which evaluates to true is found. Then the corresponding Body
(sequence of expressions) is evaluated.

The return value of Body is the return value of the when expression.

If no guard sequence is true, an if_clause run-time error will occur.

If necessary, the else branch can be used in the last branch, as that guard
sequence is always true.

.. note::

        Parenthesis around conditions are optional


Example:

.. code-block:: efene

        is_greater_than = fn (X, Y) {
            when X > Y {
                true
            }

            else {
                false
            }
        }

Difference Between *if* and *when*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See the if expression reference for details.
