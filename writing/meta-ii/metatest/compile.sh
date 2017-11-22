#!/usr/bin/env bash

set -e

cat metatest.meta | python ../meta/meta.py > metatest.py
