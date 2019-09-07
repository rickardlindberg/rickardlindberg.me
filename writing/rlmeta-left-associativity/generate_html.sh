#!/bin/sh

gen() {
    html=$(rliterate index.rliterate --html)
    echo "---"
    echo "title: '$(echo "$html" | head -n1 | cut -c52- | cut -d'<' -f1)'"
    echo "date: 2019-09-07"
    echo "tags: rlmeta"
    echo "---"
    echo "$html" | tail -n+2
}

gen > index.html
