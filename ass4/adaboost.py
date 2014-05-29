#!/usr/bin/env python
"""AdaBoost train and test functions."""

from math import sqrt, exp, log as ln


def train(samples, labels, weak_learner, max_iter=100):
    """Train adaboost on samples, and return classifier function.

    samples: X samples.
    lables: Y labels.
    weak_leaner: Function which received samples, labels, and distributions,
                 and returns a weak classifier.
    max_iter: Maximum # of iterations.
    """
    # Number of features (N form R^N). The last value is the desired label,
    # so we omit it.
    dim = len(samples)  # Dimension R^N.
    weak_classifiers = []  # Ht weak classifiers.
    weak_preds = []  # Weak predictions made by Ht classifier on samples.
    weights = []  # Weak calssifiers weights.
    distributions = [1.0 / dim] * dim  # Sample distributions.
    for t in xrange(max_iter):
        print 'iteration: %d' % t

        # Generate weak-classifier by given weak-learner.
        weak_classifier = weak_classifiers[t] = weak_learner(samples, labels, distributions)

        # Test classifier on samples.
        weak_pred = weak_preds[t] = [weak_classifier(sample) for sample in samples]

        # Sum weak classifiers' prediction errors on samples.
        error = sum(distribution for distribution, sample, label in zip(distributions, samples, labels)
                    if label != weak_pred)

        print 'error: %d' % error

        # Set updated distributions parameters.
        weight = weights[t] = 0.5 * ln((1 - error) / error)
        normalizer = 2 * sqrt(error * (1 - error))

        # Update distributions.
        distributions = [(distribution * exp(-weight * label * pred)) / normalizer
                   for distribution, pred, label in zip(distributions, weak_pred, labels)]

    return lambda x: sum(weight * classifier(x) for weight, classifier in zip(weights, weak_classifiers))
