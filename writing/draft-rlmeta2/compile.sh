#!/bin/sh

set -e

rlmeta="$1"

supportpy=$(cat support.py)
parserpy=$(cat parser.rlmeta | python "$rlmeta")
codegeneratorpy=$(cat codegenerator.rlmeta | python "$rlmeta")

cat <<EOD
import sys

$supportpy

$parserpy

$codegeneratorpy

join = "".join

def compile_grammar(grammar):
    parser = Parser()
    code_generator = CodeGenerator()
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    sys.stdout.write(compile_grammar(sys.stdin.read()))
EOD
