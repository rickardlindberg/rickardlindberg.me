#!/bin/bash

set -e

gen() {
    echo "---"
    echo "title: 'DRAFT: RLMeta Poster 2'"
    echo "date: $(date +%Y-%m-%d)"
    echo "tags: rlmeta,draft"
    echo "---"
    echo ""
    echo "**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**"
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

rm -f rlmeta-poster-2/rlmeta-*.py
rm -f rlmeta-poster-2/example*
rm -f rlmeta-poster-2/counter.py
