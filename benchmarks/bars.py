import math
import os
from textwrap import wrap

import numpy
from matplotlib import pyplot as plt



distribution_names = {
    "shuffled_16_values": "Shuffled (16 values)",
    "shuffled": "Shuffled",
    "all_equal": "All equal",
    "ascending": "Ascending",
    "descending": "Descending",
    "pipe_organ": "Pipe organ",
    "push_front": "Push front",
    "push_middle": "Push middle",
    "ascending_sawtooth": "Ascending sawtooth",
    "descending_sawtooth": "Descending sawtooth",
    "alternating": "Alternating",
    "alternating_16_values": "Alternating (16 values)"
}

for filename in os.listdir("profiles"):
    data = {}
    for line in open(os.path.join("profiles", filename)):
        size, distribution, algo, *results = line.split()
        size = int(size)
        distribution = distribution_names[distribution]
        results = [int(result) for result in results]
        if not size in data: data[size] = {}
        if not distribution in data[size]: data[size][distribution] = {}
        data[size][distribution][algo] = results

    size = 10**6
    distributions = (
        "Shuffled",
        "Shuffled (16 values)",
        "All equal",
        "Ascending",
        "Descending",
        "Pipe organ",
        "Push front",
        "Push middle",
        "Ascending sawtooth",
        "Descending sawtooth",
        "Alternating",
        "Alternating (16 values)"
    )

    algos = ("heap_sort", "pdq_sort", "quick_sort", "spread_sort", "std_sort", "verge_sort")

    groupnames = distributions
    groupsize = len(algos)
    groups = [[data[size][distribution][algo] for algo in algos] for distribution in distributions]
    barwidth = 0.6
    spacing = 1
    groupwidth = groupsize * barwidth + spacing

    colors = ["#1f77b4", "#aec7e8", "#ff7f0e", "#ffbb78", "#800080", "#3ADF00", "#800000"]
    for i, algo in enumerate(algos):
        heights = [numpy.median(data[size][distribution][algo]) for distribution in distributions]
        errors = [numpy.std(data[size][distribution][algo]) for distribution in distributions]
        plt.barh([barwidth*i + groupwidth*n for n in range(len(distributions))],
                 heights, 0.6, color = colors[i], label = algo)

    # Set axes limits and labels.
    groupnames = ['\n'.join(wrap(l, 11)) for l in groupnames]
    plt.yticks([barwidth * groupsize/2 + groupwidth*n for n in range(len(groupnames))], groupnames, horizontalalignment='center')
    plt.xlabel("Cycles per element")

    # Turn off ticks for y-axis.
    plt.tick_params(
        axis="y",
        which="both",
        left="off",
        right="off",
        labelleft="on",
        pad=45
    )

    ax = plt.gca()
    ax.invert_yaxis()
    ax.relim()
    ax.autoscale_view()
    plt.ylim(plt.ylim()[0]+1, plt.ylim()[1]-1)
    plt.legend(loc="lower right")

    plt.title("Sorting $10^{}$elements".format(round(math.log(size, 10))))

    figure = plt.gcf() # get current figure
    figure.set_size_inches(8*.75, 6*.75)
    plt.show()
    plt.savefig(os.path.join("plots", os.path.splitext(filename)[0] + ".png"), dpi = 100, bbox_inches="tight")

    plt.clf()
