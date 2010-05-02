-module(comments).
-compile(export_all).

comments() ->
    tu:test_ast("a # a comment;", "a. % a comment", same_ast_no_endl),
    tu:test_ast("# a comment\na", "% a comment\na"),
    tu:test_ast("\n# a comment\na", "\n% a comment\na"),
    tu:test_ast("\n# a comment\n\na",
        "\n% a comment\n\na"),
    tu:test_ast("\n# a comment\n# another\na",
        "\n% a comment\n% another\na"),
    tu:test_ast("\n# a comment\n\n# another\na",
        "\n% a comment\n\n% another\na"),
    tu:test_ast("  \n  # a comment  \n  \n  # another  \n a",
        "  \n  % a comment  \n  \n  % another  \na"),
    ok.

all() ->
    tu:test(?MODULE, comments),
    ok.
