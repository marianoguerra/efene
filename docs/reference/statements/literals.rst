Literal Expression
------------------

A literal expression is any expression that evaluates to a value.

Integer
~~~~~~~

A literal that represents an integer value::

        >>> 1
        1
        >>> 12
        12
        >>> 0b100011
        35
        >>> 0xf
        15
        >>> 0o12
        10

The number can be expressed in base 10 (the default), binary (0b prefix),
hexadecimal (0x prefix) or octal (0o prefix)

Float
~~~~~

A literal that represents a decimal value::

        >>> 1.2
        1.2
        >>> 32.43
        32.43

.. _scientificnotationfloat:

Scientific notation can be also used::

        >>> 12.32e-2
        0.1232
        >>> 12.32E2
        1232.0
        >>> 12.43E+2
        1243.0
        >>> 12.43e-2
        0.1243

Boolean
~~~~~~~

A literal that represents a truth value::

        >>> true
        true
        >>> false
        false

String
~~~~~~

A literal that represents a sequence of characters stored internally as a
list::

        >>> "some string"
        "some string"

A string is represented as a number representing the character as the head of a
list and a tail::

        >>> "abc"
        "abc"
        >>> [97:[98:[99]]]
        "abc"

Atom
~~~~

An atom is a literal, a constant with name. An atom should be enclosed in
single quotes (') if it does not begin with a lower-case letter or if it
contains other characters than alphanumeric characters, underscore (_), or @.

Examples:

        >>> atom
        atom
        >>> 'Atom'
        'Atom'
        >>> '§$%&%$'
        '§$%&%$'

Variable
~~~~~~~~

A term holding a value, it must start with a lower case letter followed by
lower case, upper-case, numbers or underscore characters::

        >>> Number = 12
        12
        >>> SomeValue = false
        false
        >>> A = 7
        7
        >>> B = A + Number
        19

Variables in efene once bound to a value can't change, to store the result
of a calculation you must create another variable.

Expression
~~~~~~~~~~

An expression between parenthesis is considered a literal, it will be evaluated
as one, this is useful to change the operator precedence::

        >>> 2 + 3 * 4
        14
        >>> (2 + 3) * 4
        20


Char
~~~~

A character that has the *$* sign as prefix will be evaluated to the number that
represents that character::

        >>> $a
        97
        >>> $A
        65
        >>> $(
        40
        >>> $\n
        10

List
~~~~

Compound data type with a variable number of terms::

        [Term1,...,TermN]

Each term Term in the list is called an element. The number of elements is said
to be the length of the list.

Formally, a list is either the empty list [] or consists of a head (first
element) and a tail (remainder of the list) which is also a list. The latter
can be expressed as [H:T]. The notation [Term1,...,TermN] above is actually
shorthand for the list [Term1:[...:[TermN:[]]]].

Example:

[]
         is a list, thus
[c:[]]
         is a list, thus
[b:[c:[]]]
         is a list, thus
[a:[b:[c:[]]]]
         is a list, or in short [a,b,c].

A list where the tail is a list is sometimes called a proper list. It is
allowed to have a list where the tail is not a list, for example [a:b].
However, this type of list is of little practical use.

Examples:

        >>> L1 = [a,2,(c,4)]
        [a,2,(c,4)]
        >>> [H:T] = L1
        [a,2,(c,4)]
        >>> H
        a
        >>> T
        [2,(c,4)]
        >>> L2 = [d:T]
        [d,2,(c,4)]
        >>> length(L1)
        3
        >>> length([])
        0

A collection of list processing functions can be found in the STDLIB module lists.

List Comprehension
~~~~~~~~~~~~~~~~~~

List comprehensions provide a concise way to create lists without resorting to
use of map(), filter()  and/or lambda. The resulting list definition tends
often to be clearer than lists built using those constructs. Each list
comprehension  consists of an expression followed by a for clause, then zero or
more for or if clauses. The result will be a list resulting from evaluating the
expression in the context of the for and if clauses which follow it.

::

        >>> [3 * X for X in lists.seq(1, 5)]
        [3,6,9,12,15]
        >>> [3 * X for X in lists.seq(1, 5) if X % 2 == 0]
        [6,12]
        >>> [(X, X * X) for X in lists.seq(1, 5)]
        [{1,1},{2,4},{3,9},{4,16},{5,25}]
        >>> [(X, Y) for X in lists.seq(1, 3) for Y in lists.seq(6, 8)]
        [{1,6},{1,7},{1,8},{2,6},{2,7},{2,8},{3,6},{3,7},{3,8}]


Tuple
~~~~~

Compound data type with a fixed number of terms::

        (Term1,...,TermN)

Each term Term in the tuple is called an element. The number of elements is
said to be the size of the tuple.

There exists a number of BIFs to manipulate tuples.

Examples::

        >>> P = (adam,24,(july,29)).
        (adam,24,(july,29))
        >>> element(1,P).
        adam
        >>> element(3,P).
        (july,29)
        >>> P2 = setelement(2,P,25).
        (adam,25,(july,29))
        >>> tuple_size(P).
        3
        >>> tuple_size(()).
        0

Function Reference
~~~~~~~~~~~~~~~~~~

If a function need to be passed as parameter then it must be referenced, for example
to pass the is_list/1 function::

        >>> lists.filter(fn (X) { is_boolean(X) }, [1, 2, true, 2.3, false, (1,3), []])
        [true,false]

For functions with multiple parameters this becomes a problem, we could write the same as::

        >>> lists.filter(fn erlang.is_boolean:1, [1, 2, true, 2.3, false, (1,3), []])
        [true,false]

The format of a function reference is::

        fn <module>.<function>:<arity>

Another example::

        >>> Ref = fn erlang.is_boolean:1
        #Fun<erlang.is_boolean.1>
        >>> lists.filter(Ref, [1, 2, true, 2.3, false, (1,3), []])
        [true,false]

Record Instantiation
~~~~~~~~~~~~~~~~~~~~

To create a new record the format is::

        <var-name> = <record-name>[attr1=val1, attr2=val2[, attr3=val3...]]

For example, to create a new record of the type person defined as::

        person = record(firstname, lastname, mail="none")

The instantiation is::

        P = person[firstname="mariano", lastname="guerra", mail="no@spam.com"]

Record Access
~~~~~~~~~~~~~

To access a value of a record the format is::
       
        <record-name>.<var-name>[<attr-name>] 

For example, to access the *firstname* attribute of the record created above::

        person.P[firstname]

Record Modification
~~~~~~~~~~~~~~~~~~~

To modify a record and store the result in a new variable the format is::

        <var-name> = <record-name>.<var-name>[attr1=val1[, attr2=val2, attr3=val3...]]

For example, to create a new record modifying some attributes from the P record::

        P1 = person.P[firstname="Mariano", lastname="Guerra"]

One or more attributes can be modified in the same expression.

Function Call
~~~~~~~~~~~~~

To call a function the format is::

        <function-name>([arg1, ...])

Or::

        <module-name>.<function-name>([arg1, ...])

Example::

        >>> is_number(1)
        true
        >>> erlang.is_number(1)
        true
        >>> lists.append([1,2,3], [4,5,6])
        [1,2,3,4,5,6]

The function name and module name can be expressions that evaluate to an atom
representing the name::

        >>> F = append
        append
        >>> Mod = lists
        lists
        >>> Mod.F([1,2,3], [4,5,6])
        [1,2,3,4,5,6]
        >>> Mod.append([1,2,3], [4,5,6])
        [1,2,3,4,5,6]
        >>> lists.F([1,2,3], [4,5,6])
        [1,2,3,4,5,6]

Binary
~~~~~~

bit string is used to store an area of untyped memory.

Bit Strings are expressed using the bit syntax.

Bit Strings which consists of a number of bits which is evenly divisible by
eight are called Binaries

Examples::

        >>> <[10,20]>
        <<10,20>>
        >>>  <["ABC"]>
        <<"ABC">>
        >>> <[1:1,0:1]>
        <<2:2>>

.. _macrovariables:

Macro Variables
~~~~~~~~~~~~~~~

A set of variables that are replaced at compile time, all the Macro Variables start
with the *$* sign and have more than one character in their name (to differentiate them
from the char operator).

The defined variables are:

$module
        the module name as atom
$module_string
        the module name as string
$line
        the current line as an integer
$file
        the file name as string

