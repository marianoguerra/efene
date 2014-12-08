.. _parametrized:

Parametrized Modules
--------------------

Information about parametrized modules can be found in the `original paper`__

__ http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.58.79&rep=rep1&type=pdf

The mod attribute must be the first statement in the module, the name must
match the name of the file without the file extension.

An example of a parametrized module:

.. code-block:: efene

        @mod(person) -> (Firstname, Lastname, Email)

        @public
        firstname = fn ()
            Firstname

        @public
        lastname = fn ()
            Lastname

        @public
        email = fn ()
            Email

A session example::

        1> P = person:new("bob", "sponge", "bob@nickelodeon.com").
        {person,"bob","sponge","bob@nickelodeon.com"}
        2> P:firstname().
        "bob"
        3> P:lastname().
        "sponge"
        4> P:email().
        "bob@nickelodeon.com"
        5>

