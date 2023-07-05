#!/usr/bin/env bash

set -e

./build.sh

(cd ../../ && ./bin/build)
