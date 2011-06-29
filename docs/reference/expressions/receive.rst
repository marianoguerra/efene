.. _receiveexpression:

Receive Expression
------------------

.. code-block:: efene

        # receive
        receive (Pattern1) [when (GuardSeq1)] {
                Body1
        }

.. code-block:: efene

        # multiple receives
        receive (Pattern1) [when (GuardSeq1)] {
                Body1
        }

        ...

        else receive (PatternN) [when (GuardSeqN)] {
                BodyN
        }

.. code-block:: efene

        # receive after
        receive (Pattern1) [when (GuardSeq1)] {
                Body1
        }

        after (ExprT) {
                BodyT
        }

.. code-block:: efene

        # receive/else receive/after
        receive (Pattern1) [when (GuardSeq1)] {
                Body1
        }

        ...

        else receive (PatternN) [when (GuardSeqN)] {
                BodyN
        }

        after (ExprT) {
                BodyT
        }

Receives messages sent to the process using the send operator (!). The patterns
Pattern are sequentially matched against the first message in time order in the
mailbox, then the second, and so on. If a match succeeds and the optional guard
sequence GuardSeq is true, the corresponding Body is evaluated. The matching
message is consumed, that is removed from the mailbox, while any other messages
in the mailbox remain unchanged.

The return value of Body is the return value of the receive expression.

receive never fails. Execution is suspended, possibly indefinitely, until a
message arrives that does match one of the patterns and with a true guard
sequence.

.. note::

        Parenthesis around conditions are optional

Example:

.. code-block:: efene

        wait_for_onhook = fn () {
            receive onhook {
                    disconnect()
                    idle()
            }
            else receive (connect, B) {
                    B ! (busy, self())
                    wait_for_onhook()
            }
        }

It is possible to augment the receive expression with a timeout:

.. code-block:: efene

        # receive/else receive/after
        receive (Pattern1) [when (GuardSeq1)] {
                Body1
        }

        ...

        else receive (PatternN) [when (GuardSeqN)] {
                BodyN
        }

        after (ExprT) {
                BodyT
        }

ExprT should evaluate to an integer. The highest allowed value is 0xffffffff,
that is, the value must fit in 32 bits. receive..after works exactly as
receive, except that if no matching message has arrived within ExprT
milliseconds, then BodyT is evaluated instead and its return value becomes the
return value of the receive..after expression.

Example:

.. code-block:: efene

        wait_for_onhook = fn () {
            receive onhook {
                    disconnect()
                    idle()
            }
            else receive (connect, B) {
                    B ! (busy, self())
                    wait_for_onhook()
            }
            after 60000 {
                    disconnect()
                    error()
            }
        }

There are two special cases for the timeout value ExprT:

infinity

    The process should wait indefinitely for a matching message -- this is the
    same as not using a timeout. Can be useful for timeout values that are
    calculated at run-time.

0

    If there is no matching message in the mailbox, the timeout will occur immediately. 

