-module(attrs).
-compile(export_all).

globals() ->
    tu:test_mod_ast("\n\n@@author(\"Mariano Guerra\")\n@@version((1, 2, 3))\n",
        "-module(temp).\n-export([]).\n-author(\"Mariano Guerra\").\n-version({1, 2, 3}).\n", "temp"),
    tu:test_mod_ast("\n\n@@complex([1, true, atom, \"string\", 1.2, (1, 2, [3])])\n",
        "-module(temp).\n-export([]).\n-complex([1, true, atom, \"string\", 1.2, {1, 2, [3]}]).\n", "temp"),
    ok.

locals() ->
    tu:test_mod_ast("\n\n@attr(1)\n@public\nfoo = fn () {\n ok\n}\n",
        "-module(temp).\n-export([foo/0]).\n-attr({{foo, 0}, 1}).\n\nfoo() ->\nok.\n", "temp"),
    tu:test_mod_ast("\n\n@attr(1)\n@complex([1, true, atom, \"string\", 1.2, (1, 2, [3])])\n@public\nfoo = fn () {\n ok\n}\n",
        "-module(temp).\n-export([foo/0]).\n-attr({{foo, 0}, 1}).\n-complex({{foo, 0}, [1, true, atom, \"string\", 1.2, {1, 2, [3]}]}).\n\nfoo() ->\nok.\n", "temp"),
    ok.

all() ->
    tu:test(?MODULE, globals),
    tu:test(?MODULE, locals).

