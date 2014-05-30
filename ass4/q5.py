#!/usr/bin/env python
"""Simulates perceptron and svm machine-learning tests."""

from sklearn.datasets import load_svmlight_files
from sklearn.ensemble import AdaBoostClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn import svm

from data import split_libsvm_dataset
from plot import plot_success_per_size, show


def adaboost_skin(X_train, y_train, X_test, y_test):
    """Learn the skin data sets with AdaBoost.

    X_*: Samples.
    y_*: labels.
    """
    print 'AdaBoost'

    min_iter = 1
    max_iter = 200
    steps = 30
    diff = (max_iter - min_iter) / steps
    iterations = [min_iter + diff * step for step in xrange(steps+1)]
    scores = []
    for T in iterations:

        clf = AdaBoostClassifier(
            base_estimator=DecisionTreeClassifier(max_depth=1),
            algorithm="SAMME",
            n_estimators=T)

        clf.fit(X_train.toarray(), y_train)
        scores.append(100 * clf.score(X_test.toarray(), y_test))

        print '\t%d Iterations: %.2f%%' % (T, scores[-1])

    return iterations, scores


def svm_skin(X_train, y_train, X_test, y_test):
    """Learn the skin data sets with SVM with Linear kernel.

    X_*: Samples.
    y_*: labels.
    """
    print 'SVM w/ Linear kernel'
    clf = svm.LinearSVC()
    clf.fit(X_train, y_train)
    score = 100 * clf.score(X_test.toarray(), y_test)

    print 'SVM score: %.2f%%' % score
    return score


if __name__ == '__main__':
    # `data_size` is an integer which controls how big the data set is.
    # Use none for to use the whole dataset.
    # split_libsvm_dataset(path='skin.txt', data_size=None)

    # Load train and test samples (X) + labels (y).
    X_train, y_train, X_test, y_test = load_svmlight_files(
        ('skin-train.libsvm', 'skin-test.libsvm'))

    svm_skin(X_train, y_train, X_test, y_test)

    # iterations, scores = adaboost_skin(X_train, y_train, X_test, y_test)
    # graph = plot_success_per_size(iterations, scores)
    # show()
