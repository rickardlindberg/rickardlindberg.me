#!/usr/bin/env bash

set -e

pushd rlmeta
echo "==============================================================================="
python bootstrap.py
echo "==============================================================================="
popd

pushd rlmetatest
./compile.sh
popd

cat examples.rlmetatest | python rlmetatest/rlmetatest.py
