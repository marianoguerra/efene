.. _typeexpression:
Type Definition
---------------

The type definition allows declaring sets of terms to form a particular type,
effectively forming a specific subtype of the set of all Erlang terms.
Subsequently, these types can be used to specify types of record fields and
argument and return values of functions.

Type information can be used to document function interfaces, provide more
information for bug detection tools such as Dialyzer, and can be exploited by
documentation tools such as Edoc for generating program documentation of
various forms.

For more information see the `Erlang Enhancement Proposal 8`__.

__ http://www.erlang.org/eeps/eep-0008.html

Syntax
======

To define a new type an extended for of annotation is used, the general form is::

        @type(<Type>) -> <TypeDefinition>

Where <Type> has the form::

        atom()

That is, the Type is identified by a name that must be a valid atom followed by
open and close parenthesis.

The <TypeDefinition> can be as simple as a value like::

        @type(numberFive()) -> 5
        @type(atomAtom()) -> 'atom'
        @type(emptyList()) -> []

It also can be a union of different values::

        @type(confirm()) -> yes | no
        @type(binary()) -> 1 | 0 

Referencing other types::

        @type(number()) -> integer() | float()

For more information on the builtin types see the `Erlang Enhancement Proposal 8`__.

__ http://www.erlang.org/eeps/eep-0008.html

Type declarations can also be parametrized by including type variables between
the parentheses. The syntax of type variables is the same as Erlang variables
(starts with an upper case letter). Naturally, these variables can - and should
- appear on the RHS of the definition. A concrete example appears below::

        @type(orddict(Key, Val)) -> [{Key, Val}]

Definitions
===========

Here are detailed the different ways to define a type.

Range
:::::

To define a range of integers the *..* operator::

        @type(bt_unit()) -> 1..256

Defines a type called bt_unit that is defined for integers in the range from 1 to 256.

Literals
::::::::

To define a type that consists of one or more literal values the same syntax as
in efene can se used::

        @type(bt_endian()) -> 'big' | 'little' | 'native'
        @type(confirm()) -> yes | no
        @type(binary()) -> 1 | 0 
        @type(binaryFloat()) -> 1.0 | 0.0 

Lists
:::::

To define a list of one or more types the syntax is::

        # a string is a list of char() types
        @type(string()) -> [char()] 
        # is the same as
        @type(string()) -> list(char())

        @type(numbers()) -> [integer() | float()] 


This definition allows empty lists, to define a type of the set of non-empty
proper lists whose elements are of type char()::

        # a string is a list of char() types
        @type(string()) -> [char(), ...] 

Tuples
::::::

To define a tuple of N elements with N types::

        @type(date()) -> (pos_integer(), pos_integer(), pos_integer())
        @type(time()) -> (non_neg_integer(), non_neg_integer(), non_neg_integer())
        @type(date_time()) -> (date(), time())

To define a tuple of one item or an empty tuple the syntax is the same as efene::

        @type(t10()) -> (1|2|3|foo|t9(),) | (,)

Here the one item tuple has a union of types, that means that the one item tuple can
contain any of those types as first element.

Records
:::::::

To define a type that consists of a record::

        @type(domTree()) -> domTree[,]

To define a type that consists of a record specifying the types of some (or
all) the attributes::

        @type(t24()) -> rec2[a :: t23(), b :: [atom()]]

Here *a* is the attribute name and t23() is the type of *a*.

This kind of type definition on records can be used while defining the record::

        person = record(name :: string(), age = 0 :: 0..150)

Functions
:::::::::

To define a type that consists of a record::

        # simple function definition
        @type(t16()) -> fun()
        # variable arguments and return type
        @type(t17()) -> fun((...) :: paren())
        # receives no arguments
        @type(t18()) -> fun((,) :: t17() | t16())

        @type(t19()) -> fun((t18()) :: t16())
        @type(check_fun()) -> fun((_, _) :: boolean())
        @type(method()) -> fun((term(), pid(), pid()) :: pid() | 'none')

The simplest form of a function type is *fun()*, to specify arguments and
return types the *::* operator is used, the expression before *::* are the
arguments and the expression after is the return type.

The *...* operator is used to specify any arity.

The argument types are a tuple, so to specify a function with no arguments
*(,)* is used.

To simplify the syntax functions receiving one argument can be defined with an
expression, then no comma is needed after the parenthesis.

Remote Types
::::::::::::

To specify a remote type use the same syntax as a remote call in efene::

        module.type()
