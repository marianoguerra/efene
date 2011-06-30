efene 0.9 Release Notes
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

Participate
~~~~~~~~~~~

a mailing list is available at librelist just send a mail to
efene@librelist.com to subscribe

as first mail you may send a hello world program in efene and present yourself
by saying your name, from where you are, how did you heard about efene and
anything else you would like to say. 

you can also participate by helping promote efene by trying it and posting your
thoughts on your blog/twitter/facebook/google+

of course, help on documentation or code is more than welcome

New Language Syntax
~~~~~~~~~~~~~~~~~~~

* :ref:`structs <struct>` expressions
* :ref:`fat arrow <fatarrow>` expressions

* allow new lines after opening list
* allow variables in a binary item like: *<[Val:Var/binary]>*
* source code parsing works with windows line break CR+LF
* allow lambdas defined as function parameters
* allow lightweight struct syntax in ifene
* add support for binary generators
* add support for "receive after <timeout> <block>" expression (receive without a pattern)

Syntax Changes
~~~~~~~~~~~~~~

* changed syntax to access modules from variables
* pretty print structs in the shell

License Changes
~~~~~~~~~~~~~~~

efene project is now licensed under the new BSD license, see LICENSE file for details.

Tools
~~~~~

* fnc reimplementation in C
* fnc builds on windows
* -a and -p command line arguments added to fnc (-pa -pz in erl)
* unified fnc and fn in a single binary (fnc -r to run programs)
* allow empty lines and comment lines on efene shell

Extras
~~~~~~

* new domain at http://efenelang.org
* first simple version of emacs mode for ifene
* basic TextMate support by creating a TextMate bundle

Internal Changes
~~~~~~~~~~~~~~~~

* build scripts for windows
* Add global build script to build tools, src, and libs
* check that all function clauses have the same number of arguments, print a nice error if not
* the else branch in the if expression translate to match _ instead of false to avoid an exception when the expression in the if doesn't evaluate to a boolean
* pretty print the results in the shell with efene syntax

New Modules or Functions
~~~~~~~~~~~~~~~~~~~~~~~~

* make rest parse simplified attributes, just one attribute required to describe a REST API for a function
* new struct module
* new schema module that allows to define a schema for validation using struct syntax and json schema format to validate structs
* removed obj code, libraries and examples
* started adding tests to efene libs
* third party libs bundled with efene

  * mochiweb
  * ibrowser
  * SimpleBridge

Documentation
~~~~~~~~~~~~~

* New features documented.
* fnc man pages
* documentation on how to build fnc
* Remove Go programming language dependency from README Also mention what OSX users have to do
* New efene :ref:`quickstart guide <quickstart>`

More Information
~~~~~~~~~~~~~~~~

* `Website`_
* `Documentation`_
* `Quickstart Guide`_
* `Download the latest snapshot`_
* `Central repo`_
* `Issue Tracker`_
* `Rosetta code page`_

.. _Issue Tracker: http://github.com/marianoguerra/efene/issues
.. _Central repo: http://github.com/marianoguerra/efene
.. _Documentation: http://marianoguerra.com.ar/efene/docs
.. _Quickstart Guide: http://marianoguerra.com.ar/efene/docs/quickstart.html
.. _Website: http://efenelang.org
.. _Download the latest snapshot: http://github.com/marianoguerra/efene/tarball/master
.. _Rosetta code page: http://rosettacode.org/wiki/Efene

