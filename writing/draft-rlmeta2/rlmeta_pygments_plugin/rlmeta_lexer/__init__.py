from pygments.lexer import RegexLexer
from pygments.token import *

class RLMetaLexer(RegexLexer):

    name = 'RLMeta'
    aliases = ['rlmeta']
    filenames = ['*.rlmeta']

    tokens = {
        'root': [
            (r'"', String, "string"),
            (r'\'', String.Char, "char"),
            (r'[=]', Name.Builtin),
            (r'[:]', Name.Builtin),
            (r'[~]', Name.Class),
            (r'[|]', Name.Class),
            (r'[!]', Name.Class),
            (r'[*]', Name.Class),
            (r'[?]', Name.Class),
            (r'->', Name.Builtin),
            (r'.', Text),
        ],
        'string': [
            (r'[^"\\]+', String),
            (r'\\n', String.Escape),
            (r'\\t', String.Escape),
            (r'\\\\', String.Escape),
            (r'\\"', String.Escape),
            (r'\\\'', String.Escape),
            (r'"', String, "#pop"),
        ],
        'char': [
            (r'[^\'\\]+', String.Char),
            (r'\\n', String.Escape),
            (r'\\t', String.Escape),
            (r'\\\\', String.Escape),
            (r'\\"', String.Escape),
            (r'\\\'', String.Escape),
            (r'\'', String.Char, "#pop"),
        ],
    }
