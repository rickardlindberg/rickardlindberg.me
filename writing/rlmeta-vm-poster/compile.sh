#!/bin/bash

set -e

rlmeta_compiler="$(pwd)/$1"

cd "$(dirname "$0")"

to_python_string() {
    python -c 'import sys; sys.stdout.write(repr(sys.stdin.read()))'
}

support_py_string=$(to_python_string < support.py)
support_py=$(python "$rlmeta_compiler" --support)
parser_py=$(python "$rlmeta_compiler" < parser.rlmeta)
codegenerator_py=$(python "$rlmeta_compiler" < codegenerator.rlmeta)

cat <<EOF
import sys
import pprint

SUPPORT = $support_py_string

$support_py

$parser_py

$codegenerator_py

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        try:
            sys.stdout.write(
                CodeGenerator().run(
                    "ast",
                    [Parser().run("grammar", sys.stdin.read())]
                )
            )
        except MatchError as e:
            MARKER = "\\033[0;31m<ERROR POSITION>\\033[0m"
            if isinstance(e.stream, basestring):
                stream_string = e.stream[:e.pos] + MARKER + e.stream[e.pos:]
            else:
                stream_string = pprint.pformat(e.stream)
            sys.exit("ERROR: {}\\nPOSITION: {}\\nSTREAM:\\n{}".format(
              e.message,
              e.pos,
              indent(stream_string)
            ))
EOF
