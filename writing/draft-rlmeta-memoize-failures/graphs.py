import matplotlib
import matplotlib.pyplot as plt
import numpy as np


overalls = []
single_grammars = []
labels = []

def add(name, overall, single):
    labels.append(name)
    overalls.append(overall)
    single_grammars.append(single)

add('optimized', 0.344, 0.185)
add('optimized\nfixed memo', 0.333, 0.145)
add('vm', 0.254, 0.104)
add('vm\nfixed memo', 0.241, 0.087)

ind = np.arange(len(overalls))  # the x locations for the groups
width = 0.35  # the width of the bars

fig, ax = plt.subplots()
rects1 = ax.bar(ind - width/2, overalls, width, label='Overall')
rects2 = ax.bar(ind + width/2, single_grammars, width, label='Single grammar')

ax.set_ylabel('Compilation time (s)')
ax.set_xticks(ind)
ax.set_xticklabels(labels)
ax.legend()

plt.xticks(rotation=70)

fig.tight_layout()

plt.show()
