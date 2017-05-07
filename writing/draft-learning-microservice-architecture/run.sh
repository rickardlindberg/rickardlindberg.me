#!/bin/sh

set -e

src=index.lhs
options=-threaded

build_dir=_build/$(dirname $src)
mkdir -p $build_dir
ghc --make $options $src -outputdir $build_dir -o $build_dir/out && $build_dir/out $*
