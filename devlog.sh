#!/usr/bin/env bash

ROOT=./writing

increment() {
    python -c 'import sys; print(str(int(sys.argv[1])+1).zfill(3))' "$1"
}

max_devlog() {
    find $ROOT -maxdepth 1 -type d -name 'devlog-*' | awk -F '-' '{ print $2 }' | sort -n | tail -n1
}

next_devlog() {
    increment $(max_devlog)
}

today() {
    date +%Y-%m-%d
}

new_devlog() {
    name="$1"
    number="$(next_devlog)"
    dir="$ROOT/devlog-$number-$name"
    cp -r ./post_templates/devlog "$dir"
    template="$dir/index.template.markdown"
    write_template="$dir/write.sh"
    sed -i "s/NUMBER/$number/" "$template"
    sed -i "s/TITLE/$name/" "$template"
    sed -i "s/DATE/$(today)/" "$template"
    sed -i "s/TITLE/devlog $number/" "$write_template"
    echo $dir
}

new_devlog "$1"
