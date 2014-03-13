#!/usr/bin/env python
"""This script generates m=(END-START) samples of random variables X~Ber(p),
where `p` is randomly chosen from (0,1).

It then lets each variable generate a random 0/1 integer, thus simulating
a biased coin toss.

Then it calculates an hypothesis based on the average of the coin tosses,
and calculates the difference (error) between the true `p` and the guessed `p`.

Finally, a Matlab-ish result plot is drawn on screen, with an approximation
polynomial of order ORDER on top of the results.
"""

from scipy.stats import bernoulli
import numpy as np
import matplotlib.pyplot as plt


START = 1  # Sample start range
END = 1000  # " end range
EXPERIMENTS = 50  # Number of experiments to retry.
ORDER = 3  # 'Best fit line' polynomial order.
FONT_SIZE = 18  # Ruler font size
Y_BOUND = .12  # Limit y-axis (hide very large errors).


if __name__ == '__main__':
    print __doc__
    print '\nExecuting experiments...\n'

    # Run experiment
    x = xrange(START, END)
    experiments = []
    for r in xrange(EXPERIMENTS):
        diffs = []
        for m in xrange(START, END):
            p = np.random.uniform()  # Generate random bernoulli parameter
            tosses = bernoulli.rvs(p, size=m)  # Simulate `m` coin tosses
            diffs.append(np.absolute(np.mean(tosses) - p))  # Calc error

        experiments.append(diffs)

    # Calculate average of experiments
    y = [np.mean(d) for d in zip(*experiments)]

    # Draw plot
    label_pos = np.max(y)
    plt.plot(x, y, '_')
    plt.title(r'$\mu\approx%1.5f,\ \sigma\approx%1.5f$' % (np.mean(y),
                                                           np.var(y)))
    plt.xlabel(r'$\#\ of\ Samples$', fontsize=FONT_SIZE)
    plt.ylabel(r'$|\hat{p}_m - p|$', fontsize=FONT_SIZE)
    plt.ylim((0, Y_BOUND))  # Limit y-axis

    # Plot an approximation polynomial over the original plot.
    coeff = np.polyfit(x, y, ORDER)
    y2 = np.polyval(coeff, x)

    print 'Close the plot window to exit the app.'
    plt.plot(x, y2, 'r-')
    plt.show()
