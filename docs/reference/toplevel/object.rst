Object Expression
-----------------

An object is a data structure that contain fields and allow simple
operations on them.

A object allows introspection operations like querying for the name
of the object, the names of its fields, or if the object has a given
attribute. More operations are available, see the section about
object expressions to see them in detail.

Since objects contain information about its structure at runtime
they allow to do operations that can't be done with records.

To define an object the obj atribute must be used passing the name of the
object as parameter followed by an arrow and as a tuple the fields of the
object as atoms.

An example of an object containing the information of an user:

.. code-block:: efene
        
        @obj(user) -> (username, mail, password)

.. note::
        objects are not part of the efene language. 

        objects are implemented using metaprogramming and the obj module.

For more information about how to instantiate and manipulate objects see
the section about object expressions.

