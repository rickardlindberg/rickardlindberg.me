#!/bin/bash

set -e

git-cola

../../bin/publish

git push --all
