#!/usr/bin/env bash

set -e

for i in {1..10}; do
    python3 testrunner.py
done
