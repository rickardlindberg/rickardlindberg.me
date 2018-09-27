#!/bin/sh

set -e

supportpy=$(cat support.py)
calculatorpy=$(cat calculator.rlmeta | python rlmeta.py)

cat <<EOD
import sys
from operator import add, mul

$supportpy

$calculatorpy

if __name__ == "__main__":
    calculator = Calculator()
    while True:
        line = raw_input("> ")
        print(calculator.run("expression", line))
EOD
