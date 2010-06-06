efene 0.7 Release Notes
-----------------------

About efene
~~~~~~~~~~~

efene is a programming language that runs on the erlang virtual machine.

The idea is to provide an alternative syntax to erlang that is most suitable
for people coming from languages like Java, C, C++, C#, Javascript.

The language is almost 100% compatible with erlang (and will be), the compiler
allows to translate an efene source file into a readable erlang one or compile
it directly to bytecode. It also adds some syntactic sugar in some places to
make some tasks easier.

New Language Syntax
~~~~~~~~~~~~~~~~~~~

 * The :ref:`If Expression <ifexpression>` now allows calling functions in the boolean expression,
   the if expression that behaves exactly like erlang is now called :ref:`When Expression <whenexpression>`.

 * :ref:`For Expression <forexpression>` syntax added, another way of writing list comprehensions that
   looks similar to for/in in some languages.

 * Integer division operator *//* as in python 3 added, the operator in erlang is *div*

 * :ref:`Macro Variables <macrovariables>`: $module, $module_string, $line and $file replaced during compilation 

 * :ref:`Scientific Notation for Floats <scientificnotationfloat>`

Language Syntax Changes
~~~~~~~~~~~~~~~~~~~~~~~

 * The :ref:`If Expression <ifexpression>` now allows calling functions in the boolean expression,
   the if expression that behaves exactly like erlang is now called :ref:`When Expression <whenexpression>`.

   No code should be changed since the new if expression is a superset of the old one.

Internal Changes
~~~~~~~~~~~~~~~~

 * Simplified parser

 * Object now throws the exception *{method_not_found, Arg1, Arg2}* when no function clause matches.

New Modules or Functions
~~~~~~~~~~~~~~~~~~~~~~~~

 * obj.copy function added to the obj module.
 * `validate.or_` allows to validate a field if one of the two validators returns true::

        >>> F = validate.or_(validate.string(), validate.binary())
        >>> F(4)
        false
        >>> F("asd")
        true
        >>> F(<["asd"]>)
        true

Others
~~~~~~

 * Improvements in the pygments syntax highlighter

Documentation
~~~~~~~~~~~~~

Documentation
        
        Some clarifications in the documentation based on feedback, new features documented.

Tutorial

        New sections in part 2.

More Information
~~~~~~~~~~~~~~~~

 * `Download the latest snapshot`_
 * `Website`_
 * `Documentation`_
 * `Tutorial`_
 * `Blog`_
 * `Central repo`_
 * `Issue Tracker`_
 * `Rosetta code page`_

.. _Issue Tracker: http://github.com/marianoguerra/efene/issues
.. _Central repo: http://github.com/marianoguerra/efene
.. _Blog: http://efene.tumblr.com
.. _Tutorial: http://marianoguerra.com.ar/efene/tutorial
.. _Documentation: http://marianoguerra.com.ar/efene/docs
.. _Website: http://marianoguerra.com.ar/efene
.. _Download the latest snapshot: http://github.com/marianoguerra/efene/tarball/master
.. _Rosetta code page: http://rosettacode.org/wiki/Efene

