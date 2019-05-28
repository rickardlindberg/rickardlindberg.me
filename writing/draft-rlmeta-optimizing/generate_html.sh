#!/bin/sh

gen() {
    html=$(rliterate rlmeta.rliterate --html)
    echo "---"
    echo "title: '$(echo "$html" | head -n1 | cut -c52- | cut -d'<' -f1)'"
    echo "date: 2019-06-01"
    echo "tags: rlmeta"
    echo "---"
    echo "$html" | tail -n+2
}

gen > index.html
