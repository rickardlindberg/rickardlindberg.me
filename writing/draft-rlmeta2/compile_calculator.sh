#!/bin/sh

set -e

support_py=$(python rlmeta.py --support)
calculator_py=$(cat calculator.rlmeta | python rlmeta.py)

cat <<EOD
import sys
from operator import add, mul

$support_py

$calculator_py

if __name__ == "__main__":
    calculator = Calculator(logger=sys.stderr.write)
    while True:
        line = raw_input("> ")
        print(calculator.run("expression", line))
EOD
