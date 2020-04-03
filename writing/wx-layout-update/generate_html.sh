#!/bin/sh

gen() {
    html=$(rliterate testlayout.rliterate --html)
    echo "---"
    echo "title: '$(echo "$html" | head -n1 | cut -c52- | cut -d'<' -f1)'"
    echo "date: 2020-04-03"
    echo "tags: wxpython,rliterate"
    echo "---"
    echo "$html" | tail -n+2
}

gen > index.html
