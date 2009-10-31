
class EfeneLexer(RegexLexer):
    """
    For efene source code.
    """

    name = 'Efene'
    aliases = ['fn', 'efene']
    filenames = ['*.fn']
    mimetypes = ['application/x-efene', 'text/x-efene', 'text/efene']

    flags = re.DOTALL
    tokens = {
        'root': [
            (r'\s+', Text),
            (r'#.*?\n', Comment),
            (r'[~\^\*!%&<>\|+=:;,/?\\-]+', Operator),
            (r'[{}\[\]();.]+', Punctuation),
            (r'(for|in|if)\b', Keyword),
            (r'(true|false)\b', Keyword.Constant),
            (r'[$a-zA-Z_][a-zA-Z0-9_]*', Name.Other),
            (r'[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?', Number.Float),
            (r'[0-9]+', Number.Integer),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
        ]
    }
