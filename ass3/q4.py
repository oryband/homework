#!/usr/bin/env python

import csv
from random import shuffle, uniform

import matplotlib.pyplot as plt
import numpy as np


def read_data(path, features = [], delim='\t'):
    """Opens training file and returns a float matrix,
    where each line is a vector of floats.
    The last value is a label, which is fixed from 1/2 to 1/-1.
    Returned list is randomly scrambled.

    path: Data file path.
    features: List of features indexes to extract, and abandon all others.
              If empty, extract all.
    """
    with open(path, 'rb') as f:
        data = [[float(xi) for xi in x] for x in csv.reader(f, delimiter=delim)]

    # Extract desired features.
    data = [[x[i] for i in features] for x in data] if features != [] else data

    # Fix labels from 1/0 to 1/-1
    data = [x[:-1] + [1] if x[-1] == 1 else x[:-1] + [-1] for x in data]

    shuffle(data)

    return data


def seperable_data(size):
    """Generates an easily seperated data set with 2 features."""
    data = []
    for _ in xrange(size):
        # x1, x2 = uniform(-5, 5), uniform(-5, 5)
        x1, x2 = uniform(0, 10), uniform(0, 10)
        label = 1 if x1 + x2 >= 10 else -1
        data.append([x1, x2, label])

    return data


def split_list(l, p=0.75):
    """Splits a list into two parts by percentage size.

    l: List to split.
    p: Percentage (0.0 - 1.0).
    """
    i = int(p * len(l))
    return l[:i], l[i+1:]


def tag(w, s):
    """Classifies a sample according to weights,
    by calculating if wTx >= 0 (dot product).
    """
    dp = 0  # Dot product.
    for wi, si in zip(w[:-1],s):
        dp += wi*si

    dp += w[-1]  # Add bias.

    return 1 if dp >= 0 else -1


def train(data, max_iter=100, r=0.1):
    """Trains perceptron on data, and returns a w in R^n vector.

    max_iter: Maximum # of iterations.
    r: Learning rate.
    """
    # Number of features (N form R^N). The last value is the desired label,
    # so we omit it.
    dim = len(data[0]) -1
    w = [0] * (dim+1)  # Weight vector. We add an extra dimension for the bias.

    print 'Features: %d' % dim

    for i in xrange(max_iter):  # Maximum of 100 iterations.
        err = 0
        for x in data:
            s, d = x[:-1], x[-1]  # Sample / desired label.
            if tag(w,s) != d:  # If we labeled wrong.
                for i in xrange(dim):  # Update weights.
                    w[i] += r * d * s[i]

                w[-1] += r * d  # Adjust bias.

                err += 1

        if err == 0:
            break

    print 'Iterations: %s' % (i+1)
    print 'Training errors: %s' % err

    return w


def test(data, w):
    """Tests perceptron w on data. Returns error/total percentage."""
    errors = 0

    for x in data:
        s, d = x[:-1], x[-1]
        if tag(w,s) != d:
            errors += 1

    percentage = 100.0 * errors / len(data)

    print 'Avg. test error: %.2f %%' % percentage
    return percentage


def plot(data, w, lim_avg=True):
    """Draw plot."""
    data = np.array(data)

    s_pos = np.array([x for x in data if x[-1] == 1])
    s_neg = np.array([x for x in data if x[-1] == -1])
    s_pos_x = s_pos[:,0]
    s_pos_y = s_pos[:,1]
    s_neg_x = s_neg[:,0]
    s_neg_y = s_neg[:,1]

    s_x = data[:,0]
    s_y = data[:,1]
    s_x_max = s_x.max()
    s_x_min = s_x.min()
    s_y_max = s_y.max()
    s_y_min = s_y.min()
    s_x_avg = s_x.mean()
    s_y_avg = s_y.mean()
    a, b = -w[0]/w[1], -w[-1]/w[1]
    w_x = np.linspace(s_x_min, s_x_max)
    w_y = a*w_x + b

    g_s_pos, g_s_neg, g_w = plt.plot(s_pos_x, s_pos_y, 'g+',
                                     s_neg_x, s_neg_y, 'r_',
                                     w_x, w_y, 'k--')

    # Legend
    # plt.legend((g_s_pos, g_s_neg, g_w),
    #            [r'Positive labeled points',
    #             r'Negative labeled points',
    #             r'Decision boundary'])

    plt.xlabel(r'Feature 1')
    plt.ylabel(r'Feature 2')

    # Limit axis.
    if lim_avg:
        plt.xlim((s_x_min, s_x_avg))
        plt.ylim((s_y_min, s_y_avg))
    else:
        plt.xlim((s_x_min, s_x_max))
        plt.ylim((s_y_min, s_y_max))

    print 'Close the plot window to exit.'
    plt.show()


if __name__ == '__main__':
    data = read_data('Skin_NonSkin.txt', [0, 1, 3])  # 3 is the label.
    # data = read_data('spambase.data', [0, 1, 57], delim=',')  # 3 is the label.
    # data = seperable_data(4000)

    train_data, test_data = split_list(data, 0.75)

    w = train(train_data, max_iter=10)

    p = test(test_data, w)

    plot(data, w, lim_avg=False)
