#!/bin/bash

set -e

rlmeta_compiler="$(pwd)/$1"

cd "$(dirname "$0")"

to_python_string() {
    python -c 'import sys; sys.stdout.write(repr(sys.stdin.read()))'
}

support_py=$(cat support.py)
support_py_string=$(to_python_string < support.py)
parser_py=$(python "$rlmeta_compiler" < parser.rlmeta)
codegenerator_py=$(python "$rlmeta_compiler" < codegenerator.rlmeta)

cat <<EOF
import sys
import time

SUPPORT = $support_py_string

$support_py

$parser_py

$codegenerator_py

join = "".join

def compile_grammar(grammar):
    parser = Parser()
    code_generator = CodeGenerator()
    t1 = time.time()
    ast = parser.run("grammar", grammar)
    t2 = time.time()
    sys.stderr.write("parser:   {}s\\n".format(t2-t1))
    t1 = time.time()
    code = code_generator.run("ast", ast)
    t2 = time.time()
    sys.stderr.write("codegen:  {}s\\n".format(t2-t1))
    return code

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
