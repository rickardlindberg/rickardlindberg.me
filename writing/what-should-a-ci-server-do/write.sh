#!/usr/bin/env bash

set -e

./build.sh

git add .

git commit -m 'Write.'

(cd ../../ && ./bin/publish && git push --all)
