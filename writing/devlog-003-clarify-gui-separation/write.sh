#!/usr/bin/env bash

set -e

./build.sh

git add -u .

git commit -m 'Write devlog 003.'

(cd ../../ && ./bin/publish && git push --all)
