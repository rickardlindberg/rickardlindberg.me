#!/bin/sh

set -e

ghc --make site.hs && ./site clean && ./site build
