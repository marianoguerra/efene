class EfeneLexer(RegexLexer):
    """
    For efene source code.
    """

    name = 'Efene'
    aliases = ['fn', 'efene']
    filenames = ['*.fn', '*.ifn']
    mimetypes = ['application/x-efene', 'text/x-efene', 'text/efene']

    flags = re.DOTALL
    keywords = [
        'after', 'switch', 'case', 'catch', 'else', 'fn', 'object', 'if',
        'record', 'for', 'in', 'receive', 'try', 'when',
        ]

    builtins = [ # See erlang(3) man page
        'abs', 'append_element', 'apply', 'atom_to_list', 'binary_to_list',
        'bitstring_to_list', 'binary_to_term', 'bit_size', 'bump_reductions',
        'byte_size', 'cancel_timer', 'check_process_code', 'delete_module',
        'demonitor', 'disconnect_node', 'display', 'element', 'erase', 'exit',
        'float', 'float_to_list', 'fun_info', 'fun_to_list',
        'function_exported', 'garbage_collect', 'get', 'get_keys',
        'group_leader', 'hash', 'hd', 'integer_to_list', 'iolist_to_binary',
        'iolist_size', 'is_atom', 'is_binary', 'is_bitstring', 'is_boolean',
        'is_builtin', 'is_float', 'is_function', 'is_integer', 'is_list',
        'is_number', 'is_pid', 'is_port', 'is_process_alive', 'is_record',
        'is_reference', 'is_tuple', 'length', 'link', 'list_to_atom',
        'list_to_binary', 'list_to_bitstring', 'list_to_existing_atom',
        'list_to_float', 'list_to_integer', 'list_to_pid', 'list_to_tuple',
        'load_module', 'localtime_to_universaltime', 'make_tuple', 'md5',
        'md5_final', 'md5_update', 'memory', 'module_loaded', 'monitor',
        'monitor_node', 'node', 'nodes', 'open_port', 'phash', 'phash2',
        'pid_to_list', 'port_close', 'port_command', 'port_connect',
        'port_control', 'port_call', 'port_info', 'port_to_list',
        'process_display', 'process_flag', 'process_info', 'purge_module',
        'put', 'read_timer', 'ref_to_list', 'register', 'resume_process',
        'round', 'send', 'send_after', 'send_nosuspend', 'set_cookie',
        'setelement', 'size', 'spawn', 'spawn_link', 'spawn_monitor',
        'spawn_opt', 'split_binary', 'start_timer', 'statistics',
        'suspend_process', 'system_flag', 'system_info', 'system_monitor',
        'system_profile', 'term_to_binary', 'tl', 'trace', 'trace_delivered',
        'trace_info', 'trace_pattern', 'trunc', 'tuple_size', 'tuple_to_list',
        'universaltime_to_localtime', 'unlink', 'unregister', 'whereis'
        ]

    operators = r'(\+|-|\*|/|%|<|>|=|==|!=|===|!==|<=|>=|\+\+|--|->|!|\^|\|\&|~|<<|>>)'
    word_operators = [
        'and', 'andd', 'not', 'or', 'xor', 'orr'
        ]

    atom_re = r"(?:[a-z][a-zA-Z0-9_]*|'[^\n']*[^\\]|\$[a-zA-Z][a-zA-Z0-9_]*')"

    variable_re = r'(?:[A-Z_][a-zA-Z0-9_]*)'

    base_re = r'(?:[2-9]|[12][0-9]|3[0-6])'

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'#.*?\n', Comment),
            (r'[{}\[\]():;.,@]+', Punctuation),
            (r'(true|false)\b', Keyword.Constant),
            (r'"(\\\\|\\"|[^"])*"', String.Double),
            ('(' + '|'.join(keywords) + r')\b', Keyword),
            ('(' + '|'.join(builtins) + r')\b', Name.Builtin),
            ('(' + '|'.join(word_operators) + r')\b', Operator.Word),
            (operators, Operator),
            (variable_re, Name.Variable),
            (atom_re, Name),
            (r'('+atom_re+')(\.)', bygroups(Name.Namespace, Punctuation)),
            (r'^('+atom_re+r')\s*(=)\s*(fn)\s*(\()', bygroups(Name.Function, Operator, Keyword, Punctuation)),
            (r'#'+atom_re+r'(:?\.'+atom_re+r')?', Name.Label),
        ],
        'numbers': [
            (r'(\d+\.?\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
            (r'0\d+', Number.Oct),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+', Number.Integer)
        ],
    }

