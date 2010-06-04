% switch statement and multiline expressions
compare_to_string(Result) ->
    case Result of
        lt ->
            "lower than";

        gt ->
            "greater than";

        eq ->
            "equal to";

        _ ->
            "invalid value '" ++
                atom_to_list(Result) ++
                "'"
    end.

