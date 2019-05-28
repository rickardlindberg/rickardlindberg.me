#!/bin/sh

gen() {
    echo "---"
    echo "title: 'Modifying the RLMeta metacompiler'"
    echo "date: 2019-05-18"
    echo "tags: rlmeta"
    echo "---"
    rliterate rlmeta.rliterate --html | tail -n+2
}

gen > index.html
