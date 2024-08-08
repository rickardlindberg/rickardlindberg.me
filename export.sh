#!/usr/bin/env bash

rm -rf _build/ _cache/ _site/

./bin/build

./process-export.py
