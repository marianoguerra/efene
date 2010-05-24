Switch Expression
-----------------

.. code-block:: efene

        switch Expr {
            case Pattern1 [when GuardSeq1] {
                Body1
            }

            ...

            case PatternN [when GuardSeqN] {
                BodyN
            }
        }

.. code-block:: efene

        switch Expr {
            case Pattern1 [when GuardSeq1] {
                Body1
            }

            ...

            case PatternN [when GuardSeqN] {
                BodyN
            }
            
            else {
                ElseBody
            }
        }


The expression Expr is evaluated and the patterns Pattern are sequentially
matched against the result. If a match succeeds and the optional guard sequence
GuardSeq is true, the corresponding Body is evaluated.

The return value of Body is the return value of the case expression.

If there is no matching pattern with a true guard sequence, a case_clause
run-time error will occur.

If necessary, the else branch can be used in the last branch, as that guard
sequence is always true.

Example:

.. code-block:: efene

        is_valid_signal = fn (Signal) {
            switch Signal {
                case (signal, _What, _From, _To) {
                    true
                }

                case (signal, _What, _To) {
                    true
                }

                else {
                    false
                }
            }
        }

