#!/bin/bash

set -e

cd "$(dirname "$0")"

support_py=$(python ../rlmeta/rlmeta.py --support)
calculator_py=$(python ../rlmeta/rlmeta.py < calculator.rlmeta)

cat <<EOF
from operator import add, mul

$support_py

$calculator_py

if __name__ == "__main__":
    calculator = Calculator()
    while True:
        line = raw_input("> ")
        print(calculator.run("expression", line))
EOF
