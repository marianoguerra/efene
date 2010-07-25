efene 0.8 Release Notes
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

* :ref:`spec expression <specexpression>` support (EEP 8)
* :ref:`type expression <typeexpression>` support (EEP 8)
* added :ref:`range statement <rangestatement>` *..* to specify ranges like lists:seq
* added :ref:`block statement <blockstatement>` to have full compatibility with erlang
* allow parenthesis in :ref:`for expression <forexpression>`
* allow :ref:`typed record fields <typedrecords>`
* added meta programming similar to template haskell through :ref:`[|...|] <astify>`, :ref:`$(...) <metaeval>` and :ref:`$[...] <metaevalandastify>` statemens
* allow attribute manipulation and generation by efene code.

  + objects, record, spec and type definitions are handled using this technique

Internal Changes
~~~~~~~~~~~~~~~~

* macro variables are replaced after the lexer instead of in the parser, simpler code, added example.
* make the build scripts use the local versions of fnc and fnc.sh instead of the global ones
* add tests that compare the ast of a whole file
* added tool to see the difference between two asts
* added all the type declarations found in the erlang source code and the equivalent in efene
* moved object and record definition to efene modules
* improved error reporting
* lots of new tests

New Modules or Functions
~~~~~~~~~~~~~~~~~~~~~~~~

* renamed some libs to make more sense (names try to avoid erlang lib names)

  + *d* is now *dct*
  + *l* is now *lst*

* new *rec* module allows record definition
* new *spec* module allows spec definitions
* new *type* module allows type definitions
* new *ast* module allows ast manipulation

* *obj* module allows object definition

Documentation
~~~~~~~~~~~~~

* New features documented.

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

