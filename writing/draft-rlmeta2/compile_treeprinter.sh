#!/bin/sh

set -e

support_py=$(python rlmeta.py --support)
treeprinter_py=$(cat treeprinter.rlmeta | python rlmeta.py)

cat <<EOF
import sys
import xml.sax.saxutils

$support_py

$treeprinter_py

escape = xml.sax.saxutils.escape

if __name__ == "__main__":
    calculator = TreePrinter(logger=sys.stderr.write)
    while True:
        object = input("> ")
        print(calculator.run("toHtmlList", object))
EOF
