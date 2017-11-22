#!/usr/bin/env bash

set -e

cat rlmetatest.rlmeta | python ../rlmeta/rlmeta.py > rlmetatest.py
