#!/usr/bin/env bash

./build.sh

git add .

git commit -m 'Write.'

(cd ../../ && ./bin/publish && git push --all)
