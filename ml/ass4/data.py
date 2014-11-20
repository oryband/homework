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


def read_skin_dataset(path, features=None, delim='\t'):
    """Open training file and return float samples.

    Each line is a vector of floats.
    The last value is a label, which is fixed from 1/2 to 1/-1.
    Returned samples.

    path: Data file path.
    features: List of features indexes to extract, and abandon all others.
              If empty, extract all.
    delim: csv dataset file delimeter.
    """
    with open(path, 'rb') as f:
        data = [
            [int(feature) for feature in line]
            for line in csv.reader(f, delimiter=delim)
        ]

    # Fix labels from 1/2 to 1/-1
    data = [sample[:-1] + [1] if sample[-1] == 1 else sample[:-1] + [-1]
            for sample in data]

    # Randomize data.
    shuffle(data)

    return data


def convert_dataset_to_libsvm(samples, path):
    """Convert samples and labels to libsvm format."""
    with open(path, 'wb') as f:
        for sample in samples:
            # Write sample's label.
            f.write('%d' % sample[-1])

            # Write sample's features.
            for i, feature in enumerate(sample[:-1], 1):  # Write features.
                # Convert to int if no data will be lost.
                if feature == int(feature):
                    f.write(' %d:%d' % (i, feature))
                # Else stick with float.
                else:
                    f.write(' %d:%f' % (i, sample))

            f.write('\n')


def split_libsvm_dataset(path='skin.txt', data_size=None, ratio=0.75):
    """Convert UCI's skin dataset to libsvm format."""
    samples = read_skin_dataset(path)
    samples = samples[:data_size] if data_size else samples

    train_samples, test_samples = split_list(samples, ratio)

    convert_dataset_to_libsvm(train_samples, 'skin-train.libsvm')
    convert_dataset_to_libsvm(test_samples, 'skin-test.libsvm')
