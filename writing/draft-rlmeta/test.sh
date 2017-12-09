#!/usr/bin/env bash

set -e

pushd rlmeta
echo "==============================================================================="
python bootstrap.py
echo "==============================================================================="
popd

pushd pipeline
./compile.sh
popd

pushd rlmetatest
cat compile.pipeline | python ../pipeline/pipeline.py > compile.py
python compile.py
popd

cat examples.rlmetatest | python rlmetatest/rlmetatest.py > examples.sh
bash examples.sh
