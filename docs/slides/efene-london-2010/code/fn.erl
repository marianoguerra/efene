% multiple function definition and guards
compare_to_string_guards(Result) when Result == lt ->
    "lower than";

compare_to_string_guards(Result) when Result == gt ->
    "greater than";

compare_to_string_guards (Result) when Result == eq ->
    "equal to";

compare_to_string_guards(Result) ->
    "invalid value '" ++
        atom_to_list(Result) ++
        "'".

