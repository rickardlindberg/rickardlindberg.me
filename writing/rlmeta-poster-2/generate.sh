#!/bin/bash

set -e

gen() {
    echo "---"
    echo "title: \"RLMeta poster 2: the poster that wasn't\""
    echo "date: 2022-02-12"
    echo "tags: rlmeta"
    echo "---"
    echo ""
    ./template.py index.template.markdown
}

(cd rlmeta-poster-2 && ./make.py)

git restore rlmeta-poster-2.zip

test ! -e rlmeta-poster-2-new.zip

find rlmeta-poster-2 ! -name '.*.swp' | sort | xargs zip rlmeta-poster-2-new.zip

unzip rlmeta-poster-2.zip -d a
unzip rlmeta-poster-2-new.zip -d b

if ! diff -r a/ b/; then
    mv rlmeta-poster-2-new.zip rlmeta-poster-2.zip
fi

rm -rf a/ b/ rlmeta-poster-2-new.zip

gen > index.markdown
