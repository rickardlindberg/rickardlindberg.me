#!/bin/bash

set -e

rlmeta_compiler="$(pwd)/$1"

cd "$(dirname "$0")"

to_python_string() {
    python -c 'import sys; sys.stdout.write(repr(sys.stdin.read()))'
}

support_py=$(cat support.py)
support_py_string=$(to_python_string < support.py)
echo "PARSER" 1>&2
parser_py=$(python "$rlmeta_compiler" < parser.rlmeta)
echo "CODEGENERATOR" 1>&2
codegenerator_py=$(python "$rlmeta_compiler" < codegenerator.rlmeta)

cat <<EOF
import sys
import time
import cProfile
import pstats

SUPPORT = $support_py_string

$support_py

$parser_py

$codegenerator_py

join = "".join

def profile(name, fn, *args):
    t1 = time.time()
    result = fn(*args)
    t2 = time.time()
    sys.stderr.write("{} {}s\\n".format(name, t2-t1))
    return result

def compile_grammar(grammar):
    parser = Parser()
    code_generator = CodeGenerator()
    ast = profile("parser:       ", parser.run, "grammar", grammar)
    code = profile("codegenerator:", code_generator.run, "ast", ast)
    return code

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        try:
            p = cProfile.Profile()
            p.enable()
            sys.stdout.write(compile_grammar(sys.stdin.read()))
            p.disable()
            s = StringIO()
            ps = pstats.Stats(p, stream=s).sort_stats("tottime")
            ps.print_stats(0.01)
            sys.stderr.write("\n" + s.getvalue() + "\n")
        except _MatchError as e:
            sys.stderr.write(e.describe())
            sys.exit(1)
EOF
