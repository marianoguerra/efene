-module(fn_info).
-compile(export_all).

version() -> "0.3".

banner() -> "efene " ++ version() ++ " exit with Ctrl+D".

print_banner() ->
    io:format("~s~n", [banner()]).
