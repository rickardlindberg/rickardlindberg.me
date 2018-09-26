#!/bin/sh

set -a

(
  cat support.py
  cat parser.rlmeta | python $1
  cat codegenerator.rlmeta | python $1
  cat runtime.py
) > $2

if diff $1 $2 > /dev/null; then
  echo "SAME"
fi
