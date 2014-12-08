.. _forexpression:

For Expression
--------------

.. code-block:: efene

        # simple for
        for Var in Seq
                ForBody

.. code-block:: efene

        # for with filter
        for Var in Seq if Cond
                ForBody

Parenthesis can be used

.. code-block:: efene

        # simple for
        for (Var in Seq)
                ForBody

.. code-block:: efene

        # for with filter
        for (Var in Seq if Cond)
                ForBody

The for expression allows to write loops in a way that is familiar to people
coming from mainstream languages.

The expression is another way of writing a list comprehension but allowing
a sequence of expressions in the body and not only one expression.

The expression collects the values of the ForBody and returns them as a list.

Examples:


.. code-block:: efene

    R0 = for X in lists.seq(1, 10)
        A = X + 1
        A

Result::

        R0 = [2,3,4,5,6,7,8,9,10,11]

.. code-block:: efene

    R1 = for X in lists.seq(1, 10) if X % 2 == 0
        X + 1

Result::

        R1 = [3,5,7,9,11]

.. code-block:: efene

    R2 = for X in lists.seq(1, 5)
        for Y in lists.seq(6, 10)
            (X, Y)

.. warning::

   The expression *lists.seq(6, 10)* in the inner for will be evaluated 5 times
   if it's a costly expression you should assign it to a variable outside the
   for and use it.

Result::

        R2= [[{1,6},{1,7},{1,8},{1,9},{1,10}],
             [{2,6},{2,7},{2,8},{2,9},{2,10}],
             [{3,6},{3,7},{3,8},{3,9},{3,10}],
             [{4,6},{4,7},{4,8},{4,9},{4,10}],
             [{5,6},{5,7},{5,8},{5,9},{5,10}]]

.. code-block:: efene

    R3 = for (X, Y) in lists.zip(lists.seq(1, 3), lists.seq(4, 6))
        (Y, X)

Result::

        R3 = [{4,1},{5,2},{6,3}]

