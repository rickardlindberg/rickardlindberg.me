from setuptools import setup

setup(
    name='rlmeta_lexer',
    version='0.1',
    packages=['rlmeta_lexer'],
    entry_points={
        'pygments.lexers': ['rlmeta_lexer=rlmeta_lexer:RLMetaLexer'],
    },
    zip_safe=False,
)
