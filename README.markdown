# Efene

Efene is a programming language that runs on the erlang virtual machine.

the idea is to provide an alternative syntax to Erlang that is most suitable
for people coming from languages like Java, C, C++, C#, Javascript, or Python.

Efene comes in two language dialects: Efene (Javascript style) and Ifene (Python style).

The language is almost 100% compatible with Erlang (and will be), the compiler
allows to translate an Efene source file into a readable Erlang one or compile it directly to BEAM bytecode.
It also adds some syntactic sugar in some places to make some tasks easier.

To see how it looks you can go to the [examples](http://github.com/marianoguerra/efene/tree/master/examples/) dir.

## Fast! Show me an example!

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

## That was simple, a more complex one!

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

A mailing-list is available at [librelist](http://librelist.com) just send a mail to [efene@librelist.com](efene@librelist.com) to subscribe.

As first mail you may send a hello world program in Efene and present yourself by saying your name, where you are, how did you heard about Efene and anything else you would like to say.

## Quick build instructions

    $ git clone git://github.com/marianoguerra/efene.git
    $ cd efene
    $ ./build.sh

## General build instructions

The build instructions create the Efene compiler, compile the Efene source and some libraries. To compile Efene source code, a binary of `fnc` (the Efene compiler) is provided for Linux (32 bits) and Windows. If you are on different OSor architecture (e.g. OSX) you first will need to compile `fnc` under `./tools` for your needs.

    $ git clone git://github.com/marianoguerra/efene.git
    $ cd efene
    $ pushd tools ; make ; popd
    $ pushd src ; ./build.sh ; popd
    $ pushd lib ; ./build.sh ; popd

## Examples

Additionally you can build and run the examples:

    $ cd examples
    $ ./build.sh
    $ ./run.sh

## Requirements

To create the Efene compiler you will need a C compiler like GCC or LLVM-GCC. For the main purpose you will need any recent Erlang/OTP release.

* Erlang
* (GCC or LLVM-GCC)

## Useful links

* [Download the latest snapshot](http://github.com/marianoguerra/efene/tarball/master)
* [Website](http://marianoguerra.com.ar/efene)
* [Documentation](http://marianoguerra.com.ar/efene/docs)
* [Tutorial](http://marianoguerra.com.ar/efene/tutorial)
* [Blog](http://efene.tumblr.com)
* [Central git repo](http://github.com/marianoguerra/efene) (fork this one)
* [Tracker](http://github.com/marianoguerra/efene/issues)
* [Rosetta code page](http://rosettacode.org/wiki/Efene)
