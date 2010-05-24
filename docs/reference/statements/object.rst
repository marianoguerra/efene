Object Expression
-----------------

An object is a data structure that allows to have a set of related attributes
under one structure, it provides operations to query the attributes, modify
them and get some information about the structure of the object.

Declaration
~~~~~~~~~~~

To create instances of an object the object type must be declare first.

To do that the top level object expression is used.

An example of an object containing the information of an user:

.. code-block:: efene
        
        user = object(username, mail, password)

Creation
~~~~~~~~

To create an object a call to the function that builds the object must be done.

The result should be bound to a variable.

.. code-block:: efene
    
    User = user("SpongeBob", "bob@nickelodeon.com", "jellyfish")

The function constructor is public so it can be accessed from other modules.

Getting an attribute
~~~~~~~~~~~~~~~~~~~~

To get the value of an attribute the get action on the object is used.

.. code-block:: efene
    
    UserName = User(get, username)

Setting an attribute
~~~~~~~~~~~~~~~~~~~~

To set an attribute to a new value two different forms can be used, each one
has a use.

In both cases the new object is returned and should be bound to a new variable.

As atom
:::::::

An attribute can be set using the set action as an atom.

.. code-block:: efene
    
    User1 = User(setpassword, "secret")

The first parameter is an atom starting with *set* and following with the name
of the attribute that is going to be set.

This way is useful for setting attributes in our code.

As tuple
::::::::

An attribute can be set using the set action as a two item tuple.

.. code-block:: efene
    
    User1 = User((set, password), "secret")

This way is useful when doing introspection using the fields of the object to
avoid building a new atom.

Getting the Name of the Object
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The name of the object can be obtained in two ways, as a string or as an atom.

The name of the atom is the name that the object has on its declaration,
in our current example the name would be *user* (as atom) and *"user*" (as
string).

As atom
:::::::

.. code-block:: efene
    
    Name = User(to, name)

As string
:::::::::

.. code-block:: efene
    
    Name = User(to, strname)


Getting the Fields of the Object
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The names of the fields that an object contains can be obtained in two ways, as
a tuple or as a list (useful to iterate over the fields).

As Tuple
::::::::

.. code-block:: efene

    Fields = User(to, fields)

As List
:::::::

.. code-block:: efene

    FieldsList = User(to, fieldslist)


Get Object as Record
~~~~~~~~~~~~~~~~~~~~

The object is stored internally as a record in a closure, if needed the record
representation of the object can be obtained.

.. code-block:: efene

    UserRecord = User(to, rec)

Checking if the Object has an Attribute
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When doing introspection it is useful to check if an attribute exists in the
object, to do that two ways are available, with the attribute name as an atom
or as a string.

Checking if the object has an attribute as string avoids creating new atoms,
since atoms aren't garbage collected.

As atom
:::::::

.. code-block:: efene

    HasAttribute = User(has, attribute)


As string
:::::::::

.. code-block:: efene

    HasAttribute = User(has, "attribute")

