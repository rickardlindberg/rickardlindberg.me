#!/usr/bin/env bash

set -e

./build.sh

git add -u .

git commit -m 'Write TITLE.'

(cd ../../ && ./bin/publish && git push --all)
