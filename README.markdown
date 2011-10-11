# Efene

Efene is a programming language that runs on the Erlang virtual machine.

The idea is to provide an alternative syntax for Erlang that is more comfortable
for people coming from languages like Java, C, C++, C#, Javascript, Ruby, or Python.

Efene comes in two language dialects: Efene (Ruby style) and Ifene (Python style).

The language is almost 100% compatible with Erlang (and soon will be at 100%), and your
Efene programs can easily interact with regular Erlang programs, or vice-versa. The compiler
can translate an Efene source file into readable Erlang, or compile it directly to BEAM bytecode.

## Quick! Show me an example!

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

You can see more examples in the [examples dir](http://github.com/marianoguerra/efene/tree/master/examples/).

## Using Efene in your app

You will need a recent Erlang/OTP release. If you are on a Debian-based distribution, you can install it with apt-get:

  $ sudo apt-get erlang erlang-parsetools

Create a directory for your new project, and download rebar, an Erlang build tool:

  $ mkdir -p myapp/src
  $ cd myapp
  $ wget http://bitbucket.org/basho/rebar/downloads/rebar; chmod u+x rebar

Create a file named `rebar.config` in the myapp directory:

  {deps, [
    {efene, ".*",
      {git, "git://github.com/marianoguerra/efene.git", "master"}
    },
    {rebar_efene_plugin, ".*",
      {git, "git://github.com/DavidMikeSimon/rebar_efene_plugin.git", "stable"}
    }
  ]}.

  {rebar_plugins, [
    rebar_efene_plugin
  ]}.

Create a directory named `src`, and within it create a `myapp.app.src` file:

  {application, myapp, [
    {description, "My first app ever"},
    {vsn, "0.0.1"},
  ]}.

Finally, go ahead and write a simple Efene (.fn) or Ifene (.ifn) file in
the src directory. Then tell rebar to download your dependencies and
compile everything:

  $ rebar get-deps
  $ rebar compile

And there you go! Whenever you make changes to your source code, just
run the compile command in rebar again:

  $ rebar compile

## Standalone build instructions

To build efene by itself, rather than as a dependency for your project:

    $ git clone git://github.com/marianoguerra/efene.git
    $ cd efene
    $ ./build.sh

This will also create an executable `bin/fnc`, which functions similarly to `erl` and `erlc`.

Additionally, you can build and run the examples:

    $ cd examples
    $ ./build.sh
    $ ./run.sh

## Participate

A mailing-list is available at [librelist](http://librelist.com/browser/efene/), just send a mail to [efene@librelist.com](mailto:efene@librelist.com) to subscribe.

## Requirements

To create the Efene compiler you will need a C compiler like GCC or LLVM-GCC, and any recent Erlang/OTP release.

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
