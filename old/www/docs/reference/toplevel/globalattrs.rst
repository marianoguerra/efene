Global Attributes
-----------------

A global attribute is an attribute declared in a module that contains
metadata.

It's defined prepending *@@* to an atom at the top level of a module
definition.

This attributes can be used to store metadata about the module that can be
later retrieved by using the *module_info* function in the module.

An example of global attributes:

.. code-block:: efene
        
        @@moddoc("module's documentation")
        @@version((1, 2, 0))


        hello = fn () {
                "hello world"
        }

        @@author("Mariano Guerra")

A global attribute can be defined at any point of the module as long as
it's at the top level.

The content of the attribute can be any efene basic type.

module_info Function
~~~~~~~~~~~~~~~~~~~~

When compiled all modules contain a pair of functions that are generated
automatically by the compiler.

The functions are *module_info/0* and *module_info/1*, this functions allow
to get the value of the attributes that were defined in a module.

For more information see the `module_info documentation in erlang`__.

__ http://www.erlang.org/doc/reference_manual/modules.html

Well Known Global Attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The *@@moddoc* global attribute is used by the *mod.doc* function to get
the documentation of the module.

It is recommended to use the *@@moddoc* global attribute to define the
documentation of the module, in that way the documentation of the module can
be generated automatically by the *mod.doc* function.

The global attributes can be obtained using the functions provided by the
*mod* module.
