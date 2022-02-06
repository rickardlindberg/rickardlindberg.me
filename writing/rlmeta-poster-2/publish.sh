#!/bin/bash

set -e

./generate.sh

git-cola

../../bin/publish

git push --all
