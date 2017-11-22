#!/usr/bin/env bash

set -e

pushd meta
python bootstrap.py
popd

pushd metatest
./compile.sh
popd

cat examples.metatest | python metatest/metatest.py
