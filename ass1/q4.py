#!/usr/bin/env python
"""This script generates (END - START) bernoulli random variables with
a random parameter `p`, and lets each variable generate a random 0/1
integer, thus simulating a biased coin toss.

Then it calculates an hypothesis based on the average of the coin tosses,
and calculates the difference (error) between the true `p` and
guessed `p`.

Finally, a Matlab-ish result plot is drawn on screen, with an
approximation polynomial of order ORDER on top of the results.
"""

from scipy.stats import bernoulli
import numpy as np
import matplotlib.pyplot as plt


START = 1  # Sample start range
END = 1000  # " end range
EXPERIMENTS = 50  # Number of experiments to retry.
ORDER = 3  # 'Best fit line' polynomial order.
FONT_SIZE = 18  # Ruler font size


if __name__ == '__main__':
    print __doc__
    print '\nExecuting experiments...\n'

    x = xrange(START, END)

    experiments = []
    for r in xrange(EXPERIMENTS):
        diffs = []
        for m in xrange(START, END):
            p = np.random.uniform()
            tosses = bernoulli.rvs(p, size=m)
            diffs.append(np.absolute(np.mean(tosses) - p))

        experiments.append(diffs)

    y = [np.mean(d) for d in zip(*experiments)]

    label_pos = np.max(y)

    plt.plot(x, y, '_')

    plt.title(r'$\mu\approx%1.5f,\ \sigma\approx%1.5f$' % (np.mean(y),
                                                           np.var(y)))
    plt.xlabel(r'$\#\ of\ Samples$', fontsize=FONT_SIZE)
    plt.ylabel(r'$|\hat{p}_m - p|$', fontsize=FONT_SIZE)
    plt.ylim((0, .12))

    # Plot an approximation polynomial over the original plot.
    coeff = np.polyfit(x, y, ORDER)
    y2 = np.polyval(coeff, x)

    plt.plot(x, y2, 'r-')

    print 'Close the plot window to exit the app.'
    plt.show()
