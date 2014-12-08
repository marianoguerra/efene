Local Attributes
----------------

A local attribute is an attribute declared in a module that contains
metadata about the function that is defined bellow.

It's defined prepending *@* to an atom at the top level of a module
definition.

This attributes can be used to store metadata about a function that can be
later retrieved by using the *module_info* function in the module.

An example of local attributes:

.. code-block:: efene
        
        @doc("returns the \"hello world\" string")

        @public
        hello = fn () {
                "hello world"
        }

A local attribute can be defined at any point of the module as long as
it's at the top level.

The content of the attribute can be any efene basic type.

Well Known Local Attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The *@public* local attribute is used by the compiler to export the functions
marked as such to be available outside the module where they were defined.

Other local attributes like *@doc("documentation")* are used by libraries
to do some operations on the functions, the user can use local attributes
for their own purposes.

It is recommended to use the *@doc* local attribute to define the documentation
of the function, in that way the documentation of the module can be generated
automatically by the *mod.doc* function.

The local attributes can be obtained using the functions provided by the
*mod* module.
