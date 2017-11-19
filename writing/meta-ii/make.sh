#!/usr/bin/env bash

set -e

compiler_for="$1"
written_in="$2"

cat $compiler_for.$written_in | python $written_in.py
