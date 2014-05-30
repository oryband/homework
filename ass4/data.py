#!/usr/bin/env python
"""Dataset methods."""

import csv
from random import shuffle


def split_list(l, ratio=0.75):
    """Split a list into two parts by given ratio.

    l: List to split.
    ratio: Percentage (0.0 - 1.0).
    """
    i = int(ratio * len(l))
    return l[:i], l[i:]


def read_data(path, features=None, delim='\t'):
    """Open training file and return float samples and labels.

    Each line is a vector of floats.
    The last value is a label, which is fixed from 1/2 to 1/-1.
    Returned samples and labels are randomly scrambled.

    path: Data file path.
    features: List of features indexes to extract, and abandon all others.
              If empty, extract all.
    """
    with open(path, 'rb') as f:
        data = [
            [float(xi) for xi in x] for x in csv.reader(f, delimiter=delim)
        ]

    # Extract desired features, or all of them if no features were given.
    data = [[x[i] for i in features] for x in data] if features else data

    # Fix labels from 1/2 to 1/-1
    data = [x[:-1] + [1] if x[-1] == 1 else x[:-1] + [-1] for x in data]

    # Randomize data.
    shuffle(data)

    return data[:-1], data[-1]


def convert_dataset_to_libsvm(samples, labels, path):
    """Convert samples and labels to libsvm format."""
    with open(path, 'wb') as f:
        for sample, label in zip(samples, labels):
            f.write(label)  # Write samble's label.

            # Write sample's features.
            for i, feature in enumerate(sample, 1):  # Write features.
                if feature.isdigit():  # If string can be converted to int.
                    f.write(' %d:%d' % (i, feature))
                else:  # String is float.
                    f.write(' %d:%f' % (i, sample))

            f.write('\n')


def convert_skin_to_svm(data_size=None):
    """Convert UCI's skin dataset to libsvm format."""
    samples, labels = read_data('skin.txt')
    if data_size is not None:
        samples = samples[:data_size]
        labels = labels[:data_size]

    train_samples, test_samples = split_list(samples, 0.75)
    train_labels, test_labels = split_list(labels, 0.75)

    convert_dataset_to_libsvm(train_samples, train_labels, 'skin-train.libsvm')
    convert_dataset_to_libsvm(test_samples, test_labels, 'skin-test.libsvm')
