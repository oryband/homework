#!/usr/bin/env python

import csv
from random import shuffle

import matplotlib.pyplot as plt
import numpy as np


def read_mails(path, features = []):
    """Opens training file and returns a float matrix,
    where each line represents a mail 'vector'.

    features: List of features indexes to extract, and abandon all others.
              If empty, extract all.

    Returned list is randomly scrambled.
    """
    with open(path, 'rb') as f:
        data = [[float(xi) for xi in x] for x in csv.reader(f)]

    # Extract desired features.
    data = [[x[i] for i in features] for x in data] if features != [] else data

    shuffle(data)

    return data


def divide_list(l, percentage=0.75):
    """Divides a list into two parts by percentage size."""
    index = (int(len(l) * percentage))
    return l[:index], l[index + 1:]


def tag(w, x):
    """Receives a weight vector and a sample, and classifies (labels) the
    sample according to the threshold.
    """
    threshold = 0
    for wi, xi in zip(w, x[:-1]):  # Last value is the label.
        threshold += wi * xi

    return 1 if threshold >= 0 else 0  # 1 = spam, 0 = not spam.


def train(data, max_iter = 100, learning_rate = 1):
    """Trains perceptron on data, and returns a w in R^n vector.
    max_iter: Maximum # of iterations.
    learning_rate: Gree 'Niu' letter.
    """
    # Number of features (=dimension N). The last value is the desired label,
    # so we omit it.
    dim = len(data[0]) -1
    w = [0] * dim  # Weight vector.

    print 'Features: %d' % dim

    for i in xrange(max_iter):  # Maximum of 100 iterations.
        global_err = 0

        for x in data:
            label = tag(w, x)

            if label != x[-1]:  # If we labeled wrong.
                err = x[-1] - label  # desired label - actual label

                for wi, xi in zip(w,x):
                    wi += learning_rate * err * xi

                global_err += abs(err)

        if global_err == 0:
            break

    print 'Iterations: %s' % (i+1)
    print 'Training errors: %s' % global_err

    return w


def test(data, w):
    """Tests perceptron w on data. Returns error/total percentage."""
    errors = 0

    for x in data:
        if tag(w, x) != x[-1]:
            errors += 1

    percentage = 100.0 * errors / len(data)

    print 'Avg. test error: %.2f %%' % percentage
    return percentage


if __name__ == '__main__':
    # data = read_mails('spambase.data', [3, 37, 57])  # 57 is the label.
    data = read_mails('spambase.data', [21, 44, 57])  # 57 is the label.
    train_data, test_data = divide_list(data, 0.75)
    w = train(train_data, max_iter = 100)
    percentage = test(test_data, w)

    # Draw plot
    t_ones = np.array([x for x in train_data if x[-1] == 1])
    t_zeros = np.array([x for x in train_data if x[-1] == 0])

    t0, t1 = plt.plot(
            t_ones[:, 0], t_ones[:, 1], 'g.',
            t_zeros[:, 0], t_zeros[:, 1], 'r.')

    ws, = plt.plot([w[0]*1, w[0]*2], [w[1]*1, w[1]*2], 'k-')

    # Legend
    # plt.legend((p1, p2),
    #            [r'$\hat{R}(h)\ avg.\ over\ %d\ experiments$' % EXPERIMENTS,
    #             r'$Approx.\ poly.\ (order\ %d)$' % ORDER])

    print 'Close the plot window to exit the app.'
    # plt.xlabel(r'$Samples\ (m)$', fontsize=FONT_SIZE)
    # plt.ylabel(r'$\hat{R}(h)\ avg.$', fontsize=FONT_SIZE)
    # plt.xlim((0, 0.07))  # Limit y-axis
    # plt.ylim((0, 0.07))  # Limit y-axis
    plt.show()
