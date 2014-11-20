#!/usr/bin/env python
"""Plotting Adaboost and SVM tests using matplotlib."""

import matplotlib.pyplot as plt


def plot_success_per_size(iterations, success):
    """Plot iterations / success graph."""
    g = plt.plot(iterations, success, '--ko')
    plt.xlabel(r'Iterations')
    plt.ylabel(r'Success %')

    return g


def show():
    """Show matplotlib graph, plotted beforehand.."""
    plt.show()
