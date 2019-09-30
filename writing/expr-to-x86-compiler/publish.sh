#!/bin/bash

set -e

./generate_html.sh

git-cola

../../bin/publish

git push --all
