#!/bin/bash

set -e

rlmeta_compiler="$(pwd)/$1"

cd "$(dirname "$0")"

to_python_string() {
    python -c 'import sys; sys.stdout.write(repr(sys.stdin.read()))'
}

support_py=$(cat support.py)
support_py_string=$(cat support.py | to_python_string)
parser_py=$(cat parser.rlmeta | python "$rlmeta_compiler")
codegenerator_py=$(cat codegenerator.rlmeta | python "$rlmeta_compiler")

cat <<EOF
import sys

SUPPORT = $support_py_string

$support_py

$parser_py

$codegenerator_py

join = "".join

def compile_grammar(grammar):
    parser = Parser()
    code_generator = CodeGenerator()
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        try:
            sys.stdout.write(compile_grammar(sys.stdin.read()))
        except _MatchError as e:
            sys.stderr.write(e.describe())
            sys.exit(1)
EOF
