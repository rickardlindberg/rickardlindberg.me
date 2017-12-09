#!/usr/bin/env bash

set -e

cat pipeline.rlmeta | python ../rlmeta/rlmeta.py > pipeline.py
