#!/bin/sh

rlmeta="$1"

cat <<EOD
import sys

$(cat support.py)

$(cat parser.rlmeta | python "$rlmeta")

$(cat codegenerator.rlmeta | python "$rlmeta")

join = "".join

def compile_grammar(grammar):
    parser = Parser()
    code_generator = CodeGenerator()
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    sys.stdout.write(compile_grammar(sys.stdin.read()))
EOD
