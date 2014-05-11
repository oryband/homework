#!/usr/bin/env python
"""Simulates perceptron and svm machine-learning tests."""

import svmutil as svm

from perceptron import train, test
from data import read_data, generate_sphere_data, split_list, convert_skin_to_svm
from plot import plot_data, plot_w, plot_w_legend, plot_success_per_size, show, figure


def status(train_data, test_data, error):
    """Print perceptron status message."""
    print 'Train/Test/Success: %d/%d/%.2f %%' % (len(train_data),
                                                 len(test_data),
                                                 100-error)


def simulate_increasing(data_size, margin=0.3, max_iter=100, learning_rate=0.1,
                        steps=5, start=None, end=None):
    """Simulate learning an increasing training data set.

    Generates an unseperable data set, and trains on an increasing training
    set, then tests and plots.

    start: Initial (first step) training data set size.
    end: Final (last step) training data set size.
    """
    data = generate_sphere_data(data_size, margin=margin)
    train_data, test_data = split_list(data, 0.75)

    # Initialize start/end sizes if not given.
    start = len(train_data)/steps if start is None else start
    end = len(train_data) if end is None else end

    w_colors = ['b', 'c', 'm', 'y', 'k']  # w vector (line) graph color.
    w_gs = []  # w plot graphs.
    sizes = []  # Training data set sizes.
    success = []  # Success rates according to training data set sizes.
    for i in xrange(steps):
        # Increase training data size according to iteration.
        size = start + i*end/steps
        current_train_data = train_data[:size]

        w = train(current_train_data, max_iter=max_iter, r=learning_rate)
        error = test(test_data, w)

        status(current_train_data, test_data, error)
        print

        # Record size-success statistics.
        sizes.append(size)
        success.append(100 - error)

        # Plot decision boundary.
        w_color = w_colors[i] if i < len(w_colors) else w_colors[-1]
        figure(0)
        g, = plot_w(current_train_data, w, color=w_color)
        w_gs.append(g)

    figure(0).suptitle('Test data size: %d\nMaximum iterations: %d' % (len(test_data), max_iter))
    plot_w_legend(w_gs, sizes)
    plot_data(data)

    figure(1).suptitle('Success rate according to training set size.')
    plot_success_per_size(sizes, success)

    show()


def simulate_seperable(data_size):
    """Simulate learning a completely seperable data set."""
    data = generate_sphere_data(10000, margin=0)
    train_data, test_data = split_list(data, 0.75)
    w = train(train_data, max_iter=500, r=0.01)
    error = test(test_data, w)
    status(train_data, test_data, error)

    plot_data(data)
    plot_w(data, w)
    show()


def simulate_skin(steps=5, max_iter=100, learning_rate=0.1):
    """Simulate learning skin data set."""
    data = read_data('Skin_NonSkin.txt')
    train_data, test_data = split_list(data, 0.75)

    start = len(train_data)/steps  # First step training set size.
    end = len(train_data)  # Final step training set size.

    sizes = []  # Training data set sizes.
    success = []  # Success rates according to training data set sizes.
    for i in xrange(steps):
        # Increase training data size according to iteration.
        size = start + i*end/steps
        current_train_data = train_data[:size]

        w = train(current_train_data, max_iter=max_iter, r=learning_rate)
        error = test(test_data, w)

        status(current_train_data, test_data, error)
        print

        # Record size-success statistics.
        sizes.append(size)
        success.append(100 - error)

    plot_success_per_size(sizes, success)
    show()


def simulate_skin_with_svm(data_size=None, train_params='-s 0 -t 0'):
    """Simulate learning skin data set with libsvm."""
    convert_skin_to_svm(data_size)

    train_y, train_x = svm.svm_read_problem('skin_train.svm')
    model = svm.svm_train(train_y, train_x, train_params)

    test_y, test_x = svm.svm_read_problem('skin_test.svm')
    p_label, p_acc, p_val = svm.svm_predict(test_y, test_x, model)


if __name__ == '__main__':
    simulate_seperable(data_size=10000)
    # simulate_increasing(data_size=10000, margin=0.7, max_iter=1000, learning_rate=0.01, steps=5)
    # simulate_skin(steps=10, max_iter=1000, learning_rate=0.1)
    # convert_seperable_to_svm(data_size=10000, margin=0)
    # simulate_skin_with_svm(data_size=10000, train_params='-s 0 -t 2')
