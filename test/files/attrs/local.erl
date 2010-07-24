-module(local).
-export([]).
-foo({{func, 0}, nil}).
-foo({{func, 0}, bar}).
-foo({{func, 0}, {return, bar, baz}}).
-foo({{func, 0}, {return, bar, {baz, 1}}}).

-foo({{func, 0}, [bar, baz]}).
-foo({{func, 0}, {return, [bar, baz], argh}}).

-foo({{func, 0}, [bar, true, 1, 1.5, "hi"]}).
-foo({{func, 0}, {return, [bar, true, 1, 1.5, "hi"], {[], [1,2]}}}).

func() ->
    ok.
