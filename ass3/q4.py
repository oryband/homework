#!/usr/bin/env python

import csv

import matplotlib.pyplot as plt
import numpy as np


def read_data(path, features = []):
    """Opens training file and returns a float matrix,
    where each line is a vector of floats.
    The last value is a label, which is fixed from 1/0 to 1/-1.
    Returned list is randomly scrambled.

    path: Data file path.
    features: List of features indexes to extract, and abandon all others.
              If empty, extract all.
    """
    with open(path, 'rb') as f:
        data = [[float(xi) for xi in x] for x in csv.reader(f)]

    # Extract desired features.
    data = [[x[i] for i in features] for x in data] if features != [] else data

    # Fix labels from 1/0 to 1/-1
    data = [x[:-1] + [1] if x[-1] == 1.0 else x[:-1] + [-1] for x in data]

    np.random.shuffle(data)

    return np.array(data)


def seperable_data(size):
    """Generates an easily seperated data set with 2 features."""
    data = []
    for _ in xrange(size):
        x1, x2 = np.random.uniform(0,10, 2)
        label = np.float_(1) if x1 + x2 > 10 else np.float_(-1)
        data.append(np.array([x1, x2, label]))

    return np.array(data)


def divide_list(l, percentage=0.75):
    """Divides a list into two parts by percentage size."""
    i = int(l.shape[0] * percentage)
    return l[:i], l[i+1:]


def train(data, max_iter, r = 1):
    """Trains perceptron on data, and returns a w in R^n vector.

    max_iter: Maximum # of iterations.
    r: Learning rate - usually marked by the Greek letter 'Eta'.
    """
    # Number of features (N form R^N). The last value is the desired label,
    # so we omit it.
    dim = data[0].shape[0] -1
    w = np.zeros(dim)  # Weight vector.

    print 'Features: %d' % dim

    for i in xrange(max_iter):  # Maximum of 100 iterations.
        err = 0
        for x in data:
            s, desired = x[:-1], x[-1]
            if np.sign(w.T.dot(s)) != desired:  # If we labeled wrong.
                w += r * desired * s
                err += desired

        if err == 0:
            break

    print 'Iterations: %s' % (i+1)
    print 'Training errors: %s' % err
    print 'W: %s' % w

    return w


def test(data, w):
    """Tests perceptron w on data. Returns error/total percentage."""
    errors = 0

    for x in data:
        s, desired = x[:-1], x[-1]
        if np.sign(w.T.dot(s)) != desired:
            errors += 1

    percentage = 100.0 * errors / len(data)

    print 'Avg. test error: %.2f %%' % percentage
    return percentage


def plot(data, w):
    """Draw plot."""
    x_pos = np.array([x for x in data if x[-1] == 1])
    x_neg = np.array([x for x in data if x[-1] == -1])

    # n = np.linalg.norm(w)
    # l = w/n
    # ww1 = [ww[1],-ww[0]]
    # ww2 = [-ww[1],ww[0]]

    # aa, bb = -w[1]/w[2], -w[0]/w[2]
    # plt.plot(l, aa*l+bb, 'g-', lw=2)

    g_x_pos, g_x_neg = plt.plot(x_pos[:, 0], x_pos[:, 1], 'g+',
                                x_neg[:, 0], x_neg[:, 1], 'r_')
                                     # l'k--')


    # Legend
    # plt.legend((p1, p2),
    #            [r'$\hat{R}(h)\ avg.\ over\ %d\ experiments$' % EXPERIMENTS,
    #             r'$Approx.\ poly.\ (order\ %d)$' % ORDER])

    # print 'Close the plot window to exit the app.'
    # plt.xlabel(r'$ss\ (m)$', fontsize=FONT_SIZE)
    # plt.ylabel(r'$\hat{R}(h)\ avg.$', fontsize=FONT_SIZE)

    # plt.xlim((0, 0.07))  # Limit y-axis
    # plt.ylim((0, 10))  # Limit y-axis

    plt.show()


if __name__ == '__main__':
    # data = read_data('spambase.data', [3, 37, 57])  # 57 is the label.
    # data = read_data('spambase.data', [21, 44, 57])  # 57 is the label.
    data = seperable_data(4000)

    train_data, test_data = divide_list(data, 0.75)

    w = train(train_data, 100)

    p = test(test_data, w)

    plot(data, w)
