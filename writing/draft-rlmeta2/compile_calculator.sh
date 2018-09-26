#!/bin/sh

set -a

(
  cat support.py
  cat calculator.rlmeta | python rlmeta.py
  cat calculator_runtime.py
) > calculator.py
