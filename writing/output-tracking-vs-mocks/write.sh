#!/usr/bin/env bash

set -e

./build.sh

git add -u .

git commit -m 'Write.'

(cd ../../ && ./bin/publish && git push origin gh-pages && git push)
