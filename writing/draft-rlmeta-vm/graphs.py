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

add('base', 0.756, 0.476)
add('avoid slicing', 0.608, 0.415)
add('faster string\nconcatenation', 0.603, 0.397)
add('and/or', 0.592, 0.380)
add('remove\nnewlines', 0.565, 0.343)
add('match call\nrule', 0.406, 0.220)
add('optimize\nposition', 0.392, 0.220)
add('lazy fail\nmessages', 0.370, 0.204)
add('peek', 0.351, 0.187)
add('last or\nexception', 0.344, 0.185)
add('vm', 0.254, 0.104)

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
