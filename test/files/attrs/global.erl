-module(global).
-export([]).
-foo(nil).
-foo(bar).
-foo({return, bar, baz}).
-foo({return, bar, {baz, 1}}).

-foo([bar, baz]).
-foo({return, [bar, baz], argh}).

-foo([bar, true, 1, 1.5, "hi"]).
-foo({return, [bar, true, 1, 1.5, "hi"], {[], [1,2]}}).
