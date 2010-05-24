Object Expression
-----------------

An object is a data structure that contain fields and allow simple
operations on them.

A object allows introspection operations like querying for the name
of the object, the names of its fields, or if the object has a given
attribute. More operations are available, see the section about
object expressions to see them in detail.

Since objects contain information about its structure in runtime
they allow to do operations that can't be done with records.

To define an object the name of the object must be specified followed by an
equal sign, the *object* keyword and the fields of the object between
parenthesis separated by commas.

An example of an object containing the information of an user:

.. code-block:: efene
        
        user = object(username, mail, password)

For more information about how to instantiate and manipulate objects see
the section about object expressions.

