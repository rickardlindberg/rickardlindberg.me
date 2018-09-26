#!/bin/sh
(
cat support.py
cat parser.rlmeta | python $1
cat codegenerator.rlmeta | python $1
cat runtime.py
) > $2
