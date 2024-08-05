#!/usr/bin/env bash

set -e

consume_frontmatter() {
    first_marker_seen="no"
    while true; do
        read line
        echo $line
        if [ "$line" = '---' ]; then
            if [ "$first_marker_seen" = "yes" ]; then
                return
            else
                first_marker_seen=yes
            fi
        fi
    done
}

for x in *.textile; do
    cat $x | (consume_frontmatter; pandoc -f textile -t markdown) > $(basename -s.textile $x).markdown
done
