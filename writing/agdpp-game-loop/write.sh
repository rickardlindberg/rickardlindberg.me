#!/usr/bin/env bash

set -e

./build.sh

git commit -a -m 'Write.'

(cd ../../ && ./bin/publish && git push --all)
