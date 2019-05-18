#!/bin/sh

gen() {
    echo "---"
    echo "title: 'Modifying RLMeta'"
    echo "date: 2010-05-18"
    echo "tags: rlmeta"
    echo "---"
    rliterate rlmeta.rliterate --html | tail -n+2
}

gen > index.html
