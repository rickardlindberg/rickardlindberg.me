#!/bin/sh

set -e

support_py=$(python rlmeta.py --support)
parser_py=$(python rlmeta.py < expression_parser.rlmeta)
codegenerator_py=$(python rlmeta.py < expression_codegenerator.rlmeta)

cat <<EOF
import sys

$support_py

$parser_py

$codegenerator_py

if __name__ == "__main__":
    parser = ExpressionParser(logger=sys.stderr.write)
    codegenerator = ExpressionCodegenerator(logger=sys.stderr.write)
    while True:
        line = raw_input("> ")
        ast = parser.run("expression", line)
        output = codegenerator.run("ast", ast)
        sys.stdout.write(output)
EOF
