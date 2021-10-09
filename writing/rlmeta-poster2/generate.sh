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

rm rlmeta-poster-2.zip

find rlmeta-poster-2 ! -name '.*.swp' | xargs zip rlmeta-poster-2.zip

gen > index.markdown
