.. _struct:

Struct Expression
-----------------

defining a struct literal::

       >>> Person = {name: "mariano", "age": 25}
       {name: [109, 97, 114, 105, 97, 110, 111], "age": 25}


accessing a field::
        
        >>> Person.name
        [109, 97, 114, 105, 97, 110, 111]
        >>> Person.age
        25

changing an attribute::

        >>> Person1 = Person.name := "Mariano"
        {name: [77, 97, 114, 105, 97, 110, 111], "age": 25}

calling a function defined in a struct::

        >>> Animal = {name: "bob", salute: (fn (Self) { io.format("hi! my name is ~s~n", [Self.name]) })}
        {name: [98, 111, 98]}
        >>> Animal.salute()
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
        >>> P1.age
        25

like the examples shown above a struct can be accessed like an object in any
other object oriented language by specifying the object followed by a dot and
the name of the field.  a struct can contain structs as attributes so you can
access nested attributes with the same syntax::

        Person.address.number

to add a lightweight object oriented syntax to efene you can also define and
call methods in structs, this works like python where every method of an object
receives the object itself as first argument and is by convention called
*Self*::

        >>> Animal = {name: "bob", salute: (fn (Self) { io.format("hi! my name is ~s~n", [Self.name]) })}
        >>> Animal.salute()
        hi! my name is bob

if you want to build more than one object that share the same methods you can
use the proto\_ attribute in a struct that can contain a set of methods that
can be shared between structs, in this case you create a struct with all the
data you need and add a field called proto\_ that contains all the methods. You
can later reuse this proto struct to assign to other objects.

when calling a function efene will first lookup in the struct for the method,
if not found it will look if there is a proto\_ attribute, if found it will
look for the method there. If in both cases the method is not found it will
raise an exception.

with this lookup algorithm you can override methods defined in the prototype by
creating a method with the same name in the struct, since it will look first
there it will find your version before the proto\_ version.

a simple pseudocode definition of the lookup algorithm is shown below.

For example, if we do::
    
    >>> Person.has(name)
    
the procedure in pseudocode is::
    
    if has in Person
        Person.has(name)
    else if proto_ in Person
        if has in Person.proto_
                Person.proto_.has(name)
        else
                raise not_found
    else
        raise not_found
    
    
a small example is available at examples/prototypes.ifn

as any other data structure in efene, structs are inmutable, that means that
when we modify a struct we need to assign the resulting value to another
variable::

        >>> Person1 = Person.name := "Mariano"

the name of the attributes can be atoms or strings, all access made using an
atom will lookup the attribute as string if not found, if a string is given
then the attribute will be looked up only as string. This is made to avoid
converting strings to atoms since atoms are not garbage collected. Other reason
for doing this is that JSON structs comming from the outside get keys converted
to strings, to avoid converting those unknown strings to atoms is another
reason for this convention.
