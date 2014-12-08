Frequently Asked Questions
--------------------------

Language Syntax
~~~~~~~~~~~~~~~

What is the difference between *if* and *when*?

        *when* is the erlang compatible *if*, *if* is a superset that allows
        calling functions in the condition. *when* is a guard as guards in
        function definitions. It's there like records for 100% compatibility
        with erlang, you shouldn't use it too much ;)

What is the difference between *record* and *object*?

        Records are like records in erlang, compile time constructs that are
        mapped to tuples in the bytecode, they don't allow a lot of flexibility
        at run time, you can use them for speed since they are only tuples.

        Objects are a construct that I added to allow more flexibility like
        introspection querying the name of the object, its fields and checking
        if an object has a field. This allow thinks like a generic object to
        json transformation or a mapper to mongodb that works with any object.
        That can't be done with records easily.

Implementation
~~~~~~~~~~~~~~

Does it compile to Erlang or to bytecode directly?

        It compiles to erlang code (fnc -t erl file.fn) or to bytecode directly
        (fnc file.fn)

Would you consider to rewrite Efene in Efene itself?

        In version 0.5 it was written in efene itself, 0.6 was a complete
        rewrite so it went back to erlang, I'm considering porting it again to
        efene now.

Should I expect similar performance to that of Erlang?

        You should expect the same performance in everything, the tests check
        that the expression in efene generate the same bytecode then erlang,
        see here for an example:
        http://github.com/marianoguerra/efene/blob/master/test/exprs.erl

Was it pleasant to develop it in Erlang? Have you considered the JVM or the CLR?

        I like erlang ideas and implementation, I find the syntax and documentation a
        little strange but I'm getting used to it (you just have to write a programming
        language in it to reach this level :P).

        It runs on the erlang VM because I like the basic concepts of erlang,
        share nothing, message passing, lightweight processes, distribution etc.
        
        I don't know if I can achieve that easily in another VM.

        The tools that erlang provides to do a language are really good.

Does beam offer interesting features for dynamic languages?

        You can build dynamic features in the erlang VM, reia is much more
        dynamic than efene and it's done in the erlang VM so yes, you can do
        it.
        
        Some tricks must be done but it's like dynamic languages in the JVM
        before invokedynamic.

Is concurrency and hot code swapping supported? How about performance?

        Those features are implemented by the erlang VM at a lower level
        so efene supports them by using the same things as in erlang.

        No special code required, the same performance.

Tools
~~~~~

Is there a emacs/vim plugin, any debugger facilities, other tools?

        I wrote a vim syntax highlighter, I don't know emacs so I should ask
        someone to write that. About the facilities you can use all the erlang
        facilities since efene generates almost the same code.
