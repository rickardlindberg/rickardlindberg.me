#!/bin/sh

gen() {
    html=$(rliterate testlayout.rliterate --html)
    echo "---"
    echo "title: 'DRAFT: $(echo "$html" | head -n1 | cut -c52- | cut -d'<' -f1)'"
    echo "date: $(date +%Y-%m-%d)"
    echo "tags: wxpython,rliterate,draft"
    echo "---"
    echo "<strong>This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.</strong>"
    echo "$html" | tail -n+2
}

gen > index.html
