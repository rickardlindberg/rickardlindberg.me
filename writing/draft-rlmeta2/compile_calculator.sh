#!/bin/sh

cat <<EOD
import sys
from operator import add, mul

$(cat support.py)

$(cat calculator.rlmeta | python rlmeta.py)

if __name__ == "__main__":
    calculator = Calculator()
    while True:
        line = raw_input("> ")
        print(calculator.run("expression", line))
EOD
