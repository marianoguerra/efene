# efene git

efene is a programming language that runs on the erlang virtual machine.

the idea is to provide an alternative syntax to erlang that is most suitable
for people coming from languages like Java, C, C++, C#, Javascript.

the language is almost 100% compatible with erlang (and will be), the compiler
allows to translate an efene source file into a readable erlang one or compile it directly to bytecode.
It also adds some syntactic sugar in some places to make some tasks easier.

to see how it looks you can go to the [examples dir](http://github.com/marianoguerra/efene/tree/master/examples/)

## fast! show me an example!

### efene (hello.fn)
<pre>
<code>
# public will allow accessing this function from other modules
@public
run() {
    io.format("hello world!")
}
</code>
</pre>

### compile
<pre>
<code>
fnc hello.fn
</code>
</pre>

### run (call the run function from the hello module)
<pre>
<code>
fnc -r hello run
</code>
</pre>

### ifene (hello.ifn)
<pre>
<code>
# public will allow accessing this function from other modules
@public
run()
    io.format("hello world!")
</code>
</pre>

### compile
<pre>
<code>
fnc hello.ifn
</code>
</pre>

### run (call the run function from the hello module)
<pre>
<code>
fnc -r hello run
</code>
</pre>

## that was simple, a more complex one!

### efene (demo.fn)
<pre>
<code>
# if statement
compare = fn (A, B) {
    if A < B {
        lt
    }
    else if A > B {
        gt
    }
    else {
        eq
    }
}

# switch statement and multiline expressions
compare_to_string = fn (Result) {
    switch Result {
        case lt {
            "lower than"
        }
        case gt {
            "greater than"
        }
        case eq {
            "equal to"
        }
        else {
            "invalid value '" ++
                atom_to_list(Result) ++
                "'"
        }
    }
}

# multiple function definition and guards
compare_to_string_guards = fn (Result) when Result == lt {
    "lower than"
}
fn (Result) when Result == gt {
    "greater than"
}
fn (Result) when Result == eq {
    "equal to"
}
fn (Result) {
    "invalid value '" ++
        atom_to_list(Result) ++
        "'"
}

# try/catch expression and tuples
fail = fn (Fun) {
    try {
        Fun()
    }
    catch error Error {
        ("error", Error)
    }
    catch throw Throw {
        ("throw", Throw)
    }
    catch Type Desc {
        (atom_to_list(Type), Desc)
    }
}

# pattern match
do = fn (add, A, B) {
    A + B
}
fn (mul, A, B) {
    A * B
}
fn (div, _A, 0) {
    invalid_division
}
fn (div, A, B) {
    A / B
}

# main function, made public to access it outside the module
@public
run = fn () {
    # lambda functions
    Print = fn (Expr) { io.format("~p~n", [Expr]) }

    Print(compare(1, 2))
    Print(compare(2, 1))
    Print(compare(2, 2))

    Print(compare_to_string(lt))
    Print(compare_to_string(gt))
    Print(compare_to_string(eq))
    Print(compare_to_string(this_is_an_invalid_value))

    Print(compare_to_string_guards(lt))
    Print(compare_to_string_guards(gt))
    Print(compare_to_string_guards(eq))
    Print(compare_to_string_guards(this_is_an_invalid_value))

    # call fail with a function that will fail in different ways
    Print(fail(fn () { throw("throw here") }))
    Print(fail(fn () { erlang.error("error here") }))
    Print(fail(fn () { exit("exit here") }))

    Print(do(add, 10, 2))
    Print(do(mul, 10, 2))
    Print(do(div, 10, 2))
    Print(do(div, 1, 0))
}
</code>
</pre>

### compile
<pre>
<code>
fnc demo.fn
</code>
</pre>

### run
<pre>
<code>
fnc -r demo run
</code>
</pre>

### ifene (demo.ifn)
<pre>
<code>
# if statement
compare = fn (A, B)
    if A < B
        lt
    else if A > B
        gt
    else
        eq

# switch statement and multiline expressions
compare_to_string = fn (Result)
    switch Result
        case lt
            "lower than"
        case gt
            "greater than"
        case eq
            "equal to"
        else
            "invalid value '" ++
                atom_to_list(Result) ++
                "'"

# multiple function definition and guards
compare_to_string_guards = fn (Result) when Result == lt
    "lower than"
fn (Result) when Result == gt
    "greater than"
fn (Result) when Result == eq
    "equal to"
fn (Result)
    "invalid value '" ++
        atom_to_list(Result) ++
        "'"

# try/catch expression and tuples
fail = fn (Fun)
    try
        Fun()
    catch error Error
        ("error", Error)
    catch throw Throw
        ("throw", Throw)
    catch Type Desc
        (atom_to_list(Type), Desc)

# pattern match
do = fn (add, A, B)
    A + B
fn (mul, A, B)
    A * B
fn (div, _A, 0)
    invalid_division
fn (div, A, B)
    A / B

# main function, made public to access it outside the module
@public
run = fn ()
    # lambda functions
    Print = fn (Expr) { io.format("~p~n", [Expr]) }

    Print(compare(1, 2))
    Print(compare(2, 1))
    Print(compare(2, 2))

    Print(compare_to_string(lt))
    Print(compare_to_string(gt))
    Print(compare_to_string(eq))
    Print(compare_to_string(this_is_an_invalid_value))

    Print(compare_to_string_guards(lt))
    Print(compare_to_string_guards(gt))
    Print(compare_to_string_guards(eq))
    Print(compare_to_string_guards(this_is_an_invalid_value))

    # call fail with a function that will fail in different ways
    Print(fail(fn () { throw("throw here") }))
    Print(fail(fn () { erlang.error("error here") }))
    Print(fail(fn () { exit("exit here") }))

    Print(do(add, 10, 2))
    Print(do(mul, 10, 2))
    Print(do(div, 10, 2))
    Print(do(div, 1, 0))

</code>
</pre>

### compile
<pre>
<code>
fnc demo.ifn
</code>
</pre>

### run
<pre>
<code>
fnc -r demo run
</code>
</pre>

## Participate

a mailing list is available at [librelist](http://librelist.com) just send a mail to efene@librelist.com to subscribe.

as first mail you may send a hello world program in efene and present yourself by saying your name, where you are, how did you heard about efene and anything else you would like to say.

## Build instructions

### clone the repository with the latest code
git clone git://github.com/marianoguerra/efene.git

### go to the source directory
cd efene/src/

### build the binaries
./build.sh

a binary of fnc is provided for linux (32 bits) if you are on other OS or architecture you will need the go programming language compilers installed and set up.

### go to the examples folder
cd ../examples/

### build the examples
./build.sh

### run the examples
./run.sh

## Useful links

* [Download the latest snapshot](http://github.com/marianoguerra/efene/tarball/master)
* [Website](http://marianoguerra.com.ar/efene)
* [Documentation](http://marianoguerra.com.ar/efene/docs)
* [Tutorial](http://marianoguerra.com.ar/efene/tutorial)
* [Blog](http://efene.tumblr.com)
* [Central repo](http://github.com/marianoguerra/efene) (fork this one)
* [Tracker](http://github.com/marianoguerra/efene/issues)
* [Rosetta code page](http://rosettacode.org/wiki/Efene)
