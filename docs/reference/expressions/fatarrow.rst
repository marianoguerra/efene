.. _fatarrow:

Fat Arrow Expression
--------------------

::

        First => Second
        Key => Value
        
Fat arrow expressions are syntactic sugar for a common pattern in erlang that is to use property lists as a data structure.

property lists are a list of two item tuples where the first item is the key and the second the value.

imagine the we have a function and we want to pass some options using a property list::

        [(debug, true), (debug_level, 4), (logging, file)]

fat arrows provide a way to write property lists and two item tuples in a more readable way::

        [debug => true, debug_level => 4, logging => file]

fat arrows can be also used in any place where a two item tuple is used, for example::

        fail = fn ()
            throw(always_fail => "I always fail")

        @public
        run = fn ()
            try
                fail()
            catch (Name => Desc)
                io.format("error: ~p ~s~n", [Name, Desc])

