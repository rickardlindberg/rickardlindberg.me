#!/usr/bin/env bash

set -e

./build.sh

git add -u .

git commit -m 'Write what-is-a-user-story.'

(cd ../../ && ./bin/publish && git push --all)
