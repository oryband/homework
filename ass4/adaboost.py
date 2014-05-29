#!/usr/bin/env python
"""Simple AdaBoost implementation, using perceptron as weak classifier.

No 3rd-party modules (numpy) used.
Perceptron code can be found at:
https://gist.github.com/oryband/5bcd6f05b5bcc05192df
"""

from math import copysign, exp, log as ln, sqrt
from operator import itemgetter


def sgn(x):
    """Mathemetical sign function implementation.

    return 1 for x >= 0 or -1 otherwise.
    """
    return copysign(1, x)


def min_index(l):
    """Return index of minimum value in list."""
    return min(enumerate(l), key=itemgetter(1))[0]


def transpose(matrix):
    """Return a transposed matrix."""
    return zip(*matrix)


def linear_threshold_learner(samples, labels, weights, num_of_steps=10):
    """Return a linear threshold classifier, according to weights."""
    columns = transpose(samples)
    classifiers = []
    # Generate threshold functions for each feature.
    for feature in columns:
        # Determine value range.
        minimum = min(feature)
        maximum = max(feature)

        # Generate `num_of_steps` thresholds, going from min to max.
        # Add an extra two thresholds  on both extreme sides of value range.
        step_size = float(maximum - minimum) / num_of_steps
        feature_classifiers = [lambda x: sgn(x - minimum + step_size * step)
                               for step in range(-1, num_of_steps + 2)]

        classifiers.append(feature_classifiers)

    best_feature_classifier = []
    for feature_classifiers in classifiers:
        feature_predictions = (
            sum(weight
                for sample, label, weight in zip(samples, labels, weights)
                if current_classifier(sample) != label)
            for current_classifier in feature_classifiers)

        best_feature_classifier.append(min_index(feature_predictions))

    for i in best_feature_classifier:
        feature = min(error for error, i in classifiers[i])

    threshold_step = best_feature_classifier[feature]

    return classifiers[feature][threshold_step]


def test_classifier(samples, labels, classifier):
    pass


def train(samples, labels, max_iter=100):
    """Train adaboost on samples, and return classifier function.

    Using perceptron as weak learning algorithm.
    See top of this file for details.

    samples: X samples.
    labels: Y labels.
    max_iter: Maximum # of iterations.
    """
    weak_classifiers = []  # Ht weak classifiers.
    weak_preds = []  # Weak predictions made by Ht classifier on samples.
    weights = []  # Weak calssifiers weights.
    sample_size = len(samples)
    distributions = [1.0 / sample_size] * sample_size  # Sample distributions.

    for _ in xrange(max_iter):
        # Generate weak-classifier by given weak-learner.
        weak_classifiers.append(linear_threshold_learner(samples, labels))
        classifier = weak_classifiers[-1]
        weak_error = test_classifier(samples, labels, classifier) > 50

        # If by some miracle perceptron made no errors, return it.
        # This will avoid dividing by zero when adjusting weights.
        if weak_error == 0:
            return classifier
        # Stop if weak classifier wasn't useful.
        # That is, produced more than 50% error.
        elif weak_error > 50:
            return False

        # Test classifier on samples.
        weak_preds.append([classifier(sample) for sample in samples])
        predictions = weak_preds[-1]

        # Sum weak classifiers' error distributions on samples.
        error = sum(distribution for distribution, label, prediction
                    in zip(distributions, labels, predictions)
                    if label != prediction)

        print 'error: %d' % error

        # Set updated distributions parameters.
        weights.append(0.5 * ln((1 - error) / error))
        weight = weights[-1]

        normalizer = 2 * sqrt(error * (1 - error))

        # Update distributions.
        # Note calculation is in floats because of math.exp().
        distributions = [
            (distribution * exp(-weight * label * pred)) / normalizer
            for distribution, pred, label
            in zip(distributions, predictions, labels)]

    # Return final, weighted average classifier.
    return lambda x: sgn(sum(
        weight * classifier(x)
        for weight, classifier in zip(weights, weak_classifiers)))


def test(samples, labels, classifier):
    """Test classifier on samples, and returns error/total percentage."""
    errors = sum(1 for sample, label in zip(samples, labels)
                 if classifier(sample) != label)

    return 100.0 * errors / len(samples)
