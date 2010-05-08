-module(records).
-compile(export_all).

simple() ->
    tu:test_mod_ast("\n\nperson = record(firstname=\"Mariano\", lastname=\"Guerra\", age=24)\n",
        "-module(temp).\n-export([]).\n-record(person, {firstname=\"Mariano\", lastname=\"Guerra\", age=24}).\n", "temp"),
    tu:test_mod_ast("\n\nperson = record(firstname=1.2, lastname=false, age=atom)\n",
        "-module(temp).\n-export([]).\n-record(person, {firstname=1.2, lastname=false, age=atom}).\n", "temp"),
    ok.

all() ->
    tu:test(?MODULE, simple).

