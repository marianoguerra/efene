Struct
------

defining a struct literal::

       >>> Person = {name: "mariano", "age": 25}
       {name: [109, 97, 114, 105, 97, 110, 111], "age": 25}


accessing a field::
        
        >>> @Person.name
        [109, 97, 114, 105, 97, 110, 111]
        >>> @Person.age
        25

checking if an attribute is defined in a struct::

        >>> name in @Person
        true
        >>> age in @Person
        true
        >>> "age" in @Person
        true
        >>> foo in @Person
        false

changing an attribute::

        >>> Person1 = @Person.name := "Mariano"
        {name: [77, 97, 114, 105, 97, 110, 111], "age": 25}

calling a function defined in a struct::

        >>> Animal = {name: "bob", salute: (fn (Self) { io.format("hi! my name is ~s~n", [@Self.name]) })}
        {name: [98, 111, 98]}
        >>> @Animal.salute()
        hi! my name is bob


structs are a data structure introduced in efene to express JSON-like data
similar to Javascript objects or python dictionaries and providing a
lightweight object syntax.

another benefit of structs is that they are 100% compatible with the format
produced and consumed by the mochijson module::

        >>> Person = {name: "mariano", "age": 25}
        {name: [109, 97, 114, 105, 97, 110, 111], "age": 25}
        >>> Json = mochijson.encode(Person)
        >>> io.format("~s~n", [Json])
        {"name":"mariano","age":25}
        >>> P1 = mochijson.decode(Json)
        >>> @P1.age
        25

like the examples shown above a struct can be accessed like an object in any
other object oriented language by specifying the object followed by a dot and
the name of the field.  a struct can contain structs as attributes so you can
access nested attributes with the same syntax::

        @Person.address.number

to add a lightweight object oriented syntax to efene you can also define and
call methods in structs, this works like python where every method of an object
receives the object itself as first argument and is by convention called
*Self*::

        >>> Animal = {name: "bob", salute: (fn (Self) { io.format("hi! my name is ~s~n", [@Self.name]) })}
        >>> @Animal.salute()
        hi! my name is bob

as any other data structure in efene, structs are inmutable, that means that
when we modify a struct we need to assign the resulting value to another
variable::

        >>> Person1 = @Person.name := "Mariano"
