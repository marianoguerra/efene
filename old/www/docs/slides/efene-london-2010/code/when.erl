% when statement
compare_when(A, B) ->
    if
        A < B ->
            lt;
        A > B ->
            gt;
        true ->
            eq
    end.

