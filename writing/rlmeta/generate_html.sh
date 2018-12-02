#!/bin/sh

gen() {
    echo "---"
    echo "title: 'DRAFT: RLMeta'"
    echo "date: $(date +%Y-%m-%d)"
    echo "---"
    python ~/rliterate/rliterate.py rlmeta.rliterate --html | tail -n+2
}

gen > index.html
