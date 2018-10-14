#!/bin/bash

set -e

cd "$(dirname "$0")"

support_py=$(python ../rlmeta/rlmeta.py --support)
parser_py=$(python ../rlmeta/rlmeta.py < parser.rlmeta)
codegenerator_py=$(python ../rlmeta/rlmeta.py < codegenerator.rlmeta)

cat <<EOF
import sys

$support_py

$parser_py

$codegenerator_py

if __name__ == "__main__":
    parser = Parser()
    codegenerator = CodeGenerator()
    while True:
        line = raw_input("> ")
        ast = parser.run("expression", line)
        assembly = codegenerator.run("ast", ast)
        sys.stdout.write(assembly)
EOF
