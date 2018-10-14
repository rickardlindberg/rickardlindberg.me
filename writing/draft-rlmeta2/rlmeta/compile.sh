#!/bin/bash

set -e

cd "$(dirname "$0")"

rlmeta="$1"

to_python_string() {
    python -c 'import sys; sys.stdout.write(repr(sys.stdin.read()))'
}

support_py=$(cat support.py)
support_py_string=$(cat support.py | to_python_string)
parser_py=$(cat parser.rlmeta | python "$rlmeta")
codegenerator_py=$(cat codegenerator.rlmeta | python "$rlmeta")

cat <<EOF
import sys

SUPPORT = $support_py_string

$support_py

$parser_py

$codegenerator_py

join = "".join

def compile_grammar(grammar, logger=None):
    parser = Parser(logger)
    code_generator = CodeGenerator(logger)
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        sys.stdout.write(compile_grammar(
            sys.stdin.read(),
            logger=sys.stderr.write
        ))
EOF
