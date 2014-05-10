#!/usr/bin/env python

import csv

from random import shuffle, random, choice
from math import pi, sin, cos, sqrt


def read_data(path, features=[], delim='\t'):
    """Opens training file and returns a float matrix,
    where each line is a vector of floats.
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


def generate_sphere_data(size, margin=0.2):
    """Generates a 2d circle dataset, where the center is randomly classified,
    and the borders are strictly classified.
    """
    data = []
    for _ in xrange(size):
        t = 2*pi*random()
        r = sqrt(random())
        x1, x2 = r*cos(t), r*sin(t)

        loc = x1+x2
        if loc >= margin:
            label = 1
        elif loc < -margin:
            label = -1
        else:
            label = choice((-1, 1))

        data.append((x1, x2, label))

    return data


def split_list(l, ratio=0.75):
    """Splits a list into two parts by percentage size.

    l: List to split.
    ratio: Percentage (0.0 - 1.0).
    """
    i = int(ratio * len(l))
    return l[:i], l[i:]
