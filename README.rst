efene
=====

it's alive!

Quick Start
-----------

efene integrates with rebar3 so we will start with that.

first download the app template for a basic efene app, you must do this only once::

    mkdir -p ~/.rebar3/templates && git clone https://github.com/efene/rebar3_efene_app_template.git ~/.rebar3/templates/ && rm -rf ~/.rebar3/templates/.git

then create a folder for your app, change myapp for the name of your app::

    mkdir myapp
    cd myapp
    wget https://s3.amazonaws.com/rebar3/rebar3
    chmod u+x rebar3
    ./rebar3 new fn_app name=myapp

now compile your code::

    ./rebar3 efene

you can now play with it, for now we will use the erlang shell::

    ./rebar3 shell

and call our function::

    1> myapp:hello("World").
    Hello World!ok

note: the ok after hello world is the value returned by the hello function.

License
-------

`APL 2.0 <https://www.apache.org/licenses/LICENSE-2.0.html>`_, see LICENSE file for details
