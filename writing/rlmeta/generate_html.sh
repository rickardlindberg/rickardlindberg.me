#!/bin/sh

gen() {
    echo "---"
    echo "title: 'A meta approach to implementing programming languages'"
    echo "date: 2018-12-02"
    echo "tags: rlmeta,favourite"
    echo "---"
    rliterate rlmeta.rliterate --html | tail -n+2
}

gen > index.html
