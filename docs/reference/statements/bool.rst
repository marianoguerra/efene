Bool Expression
---------------

An expression that operates on boolean values and evaluates to a boolean.

The operators available

and
~~~

Does the **and** operation on the expressions

=====  =====  =======
   Inputs     Output
------------  -------
  A      B    A and B
=====  =====  =======
false  false  false
true   false  false 
false  true   false
true   true   true
=====  =====  =======

This is the short circuit operator, that means that if the first expression
evaluates to false then the second expression is not evaluated

or
~~

Does the **or** operation on the expressions

=====  =====  ======
   Inputs     Output
------------  ------
  A      B    A or B
=====  =====  ======
false  false  false
true   false  true
false  true   true
true   true   true
=====  =====  ======

This is the short circuit operator, that means that if the first expression
evaluates to true then the second expression is not evaluated

xor
~~~

Does the **xor** operation on the expressions

=====  =====  =======
   Inputs     Output
------------  -------
  A      B    A xor B
=====  =====  =======
false  false  false
true   false  true
false  true   true
true   true   false
=====  =====  =======

not
~~~

Negates the value of the expression

=====  ======
Input  Output
-----  ------
  A    not A
=====  ======
false  true 
true   false
=====  ======

andd
~~~~

the same as **and** but it evaluates both expression no matter the value of the
first expression.

This operator is available only for compatibility with erlang, the *and* operator
should be used instead.

=====  =====  ========
   Inputs     Output
------------  --------
  A      B    A andd B
=====  =====  ========
false  false  false
true   false  false 
false  true   false
true   true   true
=====  =====  ========

Because of its precedence this operator requires the expressions to be
surrounded by parenthesis to avoid a syntax error if they are complex
expressions and not literals.

orr
~~~

The same as **or** but it evaluates both expression no mather the value of the
first expression.

This operator is available only for compatibility with erlang, the *or* operator
should be used instead.

=====  =====  =======
   Inputs     Output
------------  -------
  A      B    A orr B
=====  =====  =======
false  false  false
true   false  true
false  true   true
true   true   true
=====  =====  =======

Because of its precedence this operator requires the expressions to be
surrounded by parenthesis to avoid a syntax error if they are complex
expressions and not literals.
