Record Expression
-----------------

A record is a compile time data structure that is used to store information.

At compile time the record is translated into a tuple.

For more information about record see the `erlang documentation`__

__ http://www.erlang.org/doc/reference_manual/records.html

Records in efene are implemented to be compatible with erlang, objects are
recommended to be used instead.

To define a record the name of the record must be specified followed by an
equal sign, the *record* keyword and the fields of the record between 
parenthesis separated by commas.

An example of a record containing the information of an user:

.. code-block:: efene
        
        user = record(username="", mail="", password="")

Default values for the fields can be specified after an equal sign.

.. _typedrecords:
The type of the attribute can be defined:

.. code-block:: efene
        
        person = record(username="" :: string(), age :: integer())

For more information about how to instantiate and manipulate records see
the section about record expressions.

