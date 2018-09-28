#!/bin/sh

set -e

pygmentize -l rlmeta_lexer/__init__.py:RLMetaLexer -x ../parser.rlmeta
