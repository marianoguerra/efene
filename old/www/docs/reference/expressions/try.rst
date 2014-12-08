.. _tryexpression:

Try Expression
--------------

.. code-block:: efene

        try {
                Expres
        }

        catch ([Class1] ExceptionPattern1) [when (ExceptionGuardSeq1)] {
                ExceptionBody1
        }

        catch ([ClassN] ExceptionPatternN) [when (ExceptionGuardSeqN)] {
                ExceptionBodyN
        }

Returns the value of Exprs (a sequence of expressions Expr1, ..., ExprN) unless
an exception occurs during the evaluation. In that case the exception is caught
and the patterns ExceptionPattern with the right exception class Class are
sequentially matched against the caught exception. An omitted Class is
shorthand for throw. If a match succeeds and the optional guard sequence
ExceptionGuardSeq is true, the corresponding ExceptionBody is evaluated to
become the return value.

If an exception occurs during evaluation of Exprs but there is no matching
ExceptionPattern of the right Class with a true guard sequence, the exception
is passed on as if Exprs had not been enclosed in a try expression.

If an exception occurs during evaluation of ExceptionBody it is not caught.

.. note::

        Parenthesis around conditions are optional

The try expression can also be augmented with an after section, intended to be
used for cleanup with side effects:

.. code-block:: efene

        try {
                Expres
        }

        catch ([Class1] ExceptionPattern1) [when (ExceptionGuardSeq1)] {
                ExceptionBody1
        }

        catch ([ClassN] ExceptionPatternN) [when (ExceptionGuardSeqN)] {
                ExceptionBodyN
        }

        after {
                AfterBody
        }

AfterBody is evaluated after either Body or ExceptionBody no matter which one.
The evaluated value of AfterBody is lost; the return value of the try
expression is the same with an after section as without.

Even if an exception occurs during evaluation of Body or ExceptionBody,
AfterBody is evaluated. In this case the exception is passed on after AfterBody
has been evaluated, so the exception from the try expression is the same with
an after section as without.

If an exception occurs during evaluation of AfterBody itself it is not caught,
so if AfterBody is evaluated after an exception in Exprs, Body or
ExceptionBody, that exception is lost and masked by the exception in AfterBody.

The catch and after sections are all optional, as long as there is at least
a catch or an after section.

Example of using after, this code will close the file even in the event of
exceptions in file.read/2 or in binary_to_term/1, and exceptions will be the
same as without the try...after...end expression:

.. code-block:: efene

        termize_file = fn (Name) {
            (ok, F) = file.open(Name, [read, binary])

            try {
                (ok, Bin) = file.read(F, 1024 * 1024)
                binary_to_term(Bin)
            }
            after {
                file.close(F)
            }
        }

