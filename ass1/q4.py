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


if __name__ == '__main__':
    print __doc__
    print '\nExecuting experiments...\n'

    # Run experiment
    x = xrange(START, END)
    experiments = []
    p = np.random.uniform()  # Generate random bernoulli parameter
    for r in xrange(EXPERIMENTS):
        diffs = []
        for m in xrange(START, END):
            tosses = bernoulli.rvs(p, size=m)  # Simulate `m` coin tosses
            diffs.append(np.absolute(np.mean(tosses) - p))  # Calc error

        experiments.append(diffs)

    # Calculate average of experiments
    y = [np.mean(d) for d in zip(*experiments)]

    # Draw plot
    p1, = plt.plot(x, y, '_')

    # Approximation polynomial
    coeff = np.polyfit(x, y, ORDER)
    y2 = np.polyval(coeff, x)
    p2, = plt.plot(x, y2, 'r-')

    # Legend
    plt.legend((p1, p2),
               [r'$\hat{R}(h)\ avg.\ over\ %d\ experiments$' % EXPERIMENTS,
                r'$Approx.\ poly.\ (order\ %d)$' % ORDER])

    print 'Close the plot window to exit the app.'
    plt.xlabel(r'$Samples\ (m)$', fontsize=FONT_SIZE)
    plt.ylabel(r'$\hat{R}(h)\ avg.$', fontsize=FONT_SIZE)
    plt.ylim((0, np.mean(y) + .07))  # Limit y-axis
    plt.show()
