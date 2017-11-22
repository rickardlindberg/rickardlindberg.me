#!/usr/bin/env bash

set -e

pushd rlmeta
python bootstrap.py
popd

pushd rlmetatest
./compile.sh
popd

cat examples.rlmetatest | python rlmetatest/rlmetatest.py
