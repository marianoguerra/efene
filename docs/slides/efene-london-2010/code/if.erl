% if statement
compare_if(A, B) ->
    case A < B of
        true ->
            lt;
        false ->
            case A > B of
                true ->
                    gt;
                false ->
                    eq
            end
    end.

