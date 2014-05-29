#!/usr/bin/env python
"""Data set methods."""

import csv

from random import shuffle, random, uniform, choice
from math import pi, sin, cos, sqrt


def read_data(path, features=[], delim='\t'):
    """Open training file and returns a float matrix.

    Each line is a vector of floats.
    The last value is a label, which is fixed from 1/2 to 1/-1.
    Returned list is randomly scrambled.

    path: Data file path.
    features: List of features indexes to extract, and abandon all others.
              If empty, extract all.
    """
    with open(path, 'rb') as f:
        data = [
            [float(xi) for xi in x] for x in csv.reader(f, delimiter=delim)
        ]

    # Extract desired features.
    data = [[x[i] for i in features] for x in data] if features != [] else data

    # Fix labels from 1/0 to 1/-1
    data = [x[:-1] + [1] if x[-1] == 1 else x[:-1] + [-1] for x in data]

    shuffle(data)

    return data


def generate_sphere_data(size, margin=0.2, radius=1):
    """Generate a 2d circle dataset.

    The center is randomly classified, and the borders are strictly classified.
    """
    data = []
    for _ in xrange(size):
        t = 2*pi*random()
        r = sqrt(uniform(0, radius))
        x1, x2 = r*cos(t), r*sin(t)

        loc = x1+x2
        if loc >= margin:
            label = 1
        elif loc < -margin:
            label = -1
        else:
            label = choice((-1, 1))

        data.append([x1+1, x2+1, label])

    return data


def split_list(l, ratio=0.75):
    """Split a list into two parts by given ratio.

    l: List to split.
    ratio: Percentage (0.0 - 1.0).
    """
    i = int(ratio * len(l))
    return l[:i], l[i:]


def convert_data_to_libsvm(data, path):
    """Convert data matrix to libsvm format."""
    with open(path, 'wb') as f:
        for x in data:
            f.write('%d' % x[-1])  # Write label.

            for i in xrange(len(x) - 1):  # Write features.
                if (x[i] == int(x[i])):
                    f.write(' %d:%d' % (i+1, x[i]))
                else:
                    f.write(' %d:%f' % (i+1, x[i]))

            f.write('\n')


def convert_seperable_to_svm(data_size=10000, margin=0, radius=1):
    """Convert seperable data to libsvm format."""
    data = generate_sphere_data(4000, margin=0.7, radius=radius)
    convert_data_to_libsvm(data, 'seperable.svm')


def convert_skin_to_svm(data_size=None):
    """Convert skin data to libsvm format."""
    data = read_data('Skin_NonSkin.txt')
    if data_size is not None:
        data = data[:data_size]

    train_data, test_data = split_list(data, 0.75)
    convert_data_to_libsvm(train_data, 'skin_train.svm')
    convert_data_to_libsvm(test_data, 'skin_test.svm')
