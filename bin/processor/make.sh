#!/usr/bin/env bash

set -e

python rlmeta/rlmeta.py \
    --support \
    --compile processor.rlmeta \
    --copy main.py \
    > processor.py
