-module(module).
-compile(export_all).

module() ->
    tu:test_mod_ast("\n\nfoo = fn () {\n ok\n}\n",
        "-module(temp).\n-export([]).\nfoo() ->\nok.\n", "temp"),
    tu:test_mod_ast("\n@public\nfoo = fn () {\n ok\n}\n",
        "-module(temp).\n-export([foo/0]).\nfoo() ->\nok.\n", "temp"),
    tu:test_mod_ast("\n@public\nfoo = fn (0) {\n ok\n}\nfn (A) {\n A\n}\n",
        "-module(temp).\n-export([foo/1]).\nfoo(0) ->\nok;\n\nfoo(A) ->\n A.\n", "temp"),
    ok.

all() ->
    tu:test(?MODULE, module).
