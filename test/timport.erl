-module(timport).
-compile(export_all).

lib_abs_path() ->
    Path = import:lib_abs_path("eunit/include/eunit.hrl"),
    Expected = code:lib_dir("eunit") ++ "/include/eunit.hrl",
    tu:same(Path, Expected, true).

lib_abs_path_inexistent() ->
    Path = import:lib_abs_path("asd/include/asd.hrl"),
    Expected = {error, bad_name},
    tu:same(Path, Expected, true).

all() ->
    tu:test(?MODULE, lib_abs_path),
    tu:test(?MODULE, lib_abs_path_inexistent).
