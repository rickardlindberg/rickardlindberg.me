#!/bin/bash

set -e

CLONE_PATH=/tmp/rlmeta-perf
GRAPH_FILE=/tmp/rlmeta-perf.py

measure_overall() {
    (time ./compile.sh rlmeta.py > /dev/null) 2>&1 | ack --output='$1' 'real.*0m(.*)s$'
}

measure_single_grammar() {
    python -m cProfile -s tottime rlmeta.py < parser.rlmeta | ack --output='$1' 'calls.*in (.*) seconds'
}

avg_med_numbers() {
    python <(cat <<EOF
import sys

numbers = [float(x) for x in sys.stdin.read().splitlines()]

avg = sum(numbers)/len(numbers);
med = sorted(numbers)[(len(numbers)-1)/2] # Assumes odd number of numbers

sys.stderr.write("Numbers = {}\n".format(numbers))
sys.stderr.write(" Average = {}\n".format(avg))
sys.stderr.write("  Median = {}\n".format(med))
sys.stdout.write("{},\n{},\n".format(avg, med))
EOF
)
}

measure_all() {
    echo "add(" | tee --append "$GRAPH_FILE"
    echo "'$1'," | tee --append "$GRAPH_FILE"
    echo "Overall:"
    (
        measure_overall
        measure_overall
        measure_overall
        measure_overall
        measure_overall
        measure_overall
        measure_overall
        measure_overall
        measure_overall
        measure_overall
        measure_overall
        measure_overall
        measure_overall
        measure_overall
        measure_overall
    ) | avg_med_numbers | tee --append "$GRAPH_FILE"
    echo "Single grammar:"
    (
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
        measure_single_grammar
    ) | avg_med_numbers | tee --append "$GRAPH_FILE"
    echo ")" | tee --append "$GRAPH_FILE"
}

measure_on_commit() {
    (cd "$CLONE_PATH" && git checkout "$1" && cd "$2" && measure_all "$3")
}

rm -rf "$CLONE_PATH"

git clone ~/rickardlindberg.me/ "$CLONE_PATH"

cat <<EOF > "$GRAPH_FILE"
import matplotlib
import matplotlib.pyplot as plt
import numpy as np


overalls_avg = []
overalls_med = []
single_grammars_avg = []
single_grammars_med = []
labels = []

def add(name, overall_avg, overall_med, single_avg, single_med):
    labels.append(name)
    overalls_avg.append(overall_avg)
    overalls_med.append(overall_med)
    single_grammars_avg.append(single_avg)
    single_grammars_med.append(single_med)
EOF
measure_on_commit "c5d035acedfa3485d178a453ef2c887954478e44" "writing/rlmeta-vm" "vm original"
measure_on_commit "6b933e64e4d92809fd4464b8f9f6d2d6db6b65ad" "writing/rlmeta-memoize-failures/vm" "vm memoizing"
measure_on_commit "1db522231ae28add05ee22d1273b6d9a27933505" "writing/rlmeta-poster2" "poster v1"
measure_on_commit "5154583e9d98c123630fb41664aa6906d4801d05" "writing/rlmeta-poster2" "label action"
measure_all "tip"
cat <<EOF >> "$GRAPH_FILE"
ind = np.arange(len(overalls_avg))  # the x locations for the groups
width = 0.15  # the width of the bars
padding = 0.02

fig, ax = plt.subplots()
ax.bar(ind - 1.5*width-padding, overalls_avg, width, label='Overall (avg)')
ax.bar(ind - width/2-padding, overalls_med, width, label='Overall (med)')
ax.bar(ind + width/2+padding, single_grammars_avg, width, label='Single grammar (avg)')
ax.bar(ind + 1.5*width+padding, single_grammars_med, width, label='Single grammar (med)')

ax.plot(overalls_avg)
ax.plot(overalls_med)
ax.plot(single_grammars_avg)
ax.plot(single_grammars_med)


ax.set_ylabel('Compilation time (s)')
ax.set_xticks(ind)
ax.set_xticklabels(labels)
ax.legend()

plt.xticks(rotation=70)

fig.tight_layout()

plt.show()
EOF

python "$GRAPH_FILE"
