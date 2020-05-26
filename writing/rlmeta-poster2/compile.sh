#!/bin/bash

set -e

rlmeta_compiler="$1"

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
            stream = e.stream
            for pos in e.pos[:-1]:
                stream = stream[pos]
            pos = e.pos[-1]
            MARKER = "\\033[0;31m<ERROR POSITION>\\033[0m"
            if isinstance(stream, basestring):
                stream_string = stream[:pos] + MARKER + stream[pos:]
            else:
                stream_string = pprint.pformat(stream)
            sys.exit("ERROR: {}\\nPOSITION: {}\\nSTREAM:\\n{}".format(
                e.message,
                pos,
                indent(stream_string)
            ))
EOF
