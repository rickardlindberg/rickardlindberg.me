#!/bin/sh

set -e

rlmeta="$1"

supportpy=$(cat support.py)
supportpypy=$(cat support.py | python -c 'import sys; sys.stdout.write(repr(sys.stdin.read()))')
parserpy=$(cat parser.rlmeta | python "$rlmeta")
codegeneratorpy=$(cat codegenerator.rlmeta | python "$rlmeta")

cat <<EOD
import sys

SUPPORT = $supportpypy

$supportpy

$parserpy

$codegeneratorpy

join = "".join

def compile_grammar(grammar, logger=None):
    parser = Parser(logger)
    code_generator = CodeGenerator(logger)
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        sys.stdout.write(compile_grammar(
            sys.stdin.read(),
            logger=sys.stderr.write
        ))
EOD
