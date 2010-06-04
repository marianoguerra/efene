% try/catch expression and tuples
fail(Fun) ->
    try
        Fun()
    catch
        error:Error ->
            ("error", Error);

        throw:Throw ->
            ("throw", Throw);

        Type:Desc ->
            (atom_to_list(Type), Desc)
    end.

