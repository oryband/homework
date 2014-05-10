#!/usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np


def plot_data(data, avg=False):
    """Plots 2d data."""
    data = np.array(data)

    s_pos = np.array([x for x in data if x[-1] == 1])
    s_neg = np.array([x for x in data if x[-1] == -1])

    s_x = data[:, 0]
    s_y = data[:, 1]

    s_pos_x = s_pos[:, 0]
    s_pos_y = s_pos[:, 1]

    s_neg_x = s_neg[:, 0]
    s_neg_y = s_neg[:, 1]

    s_x_max = s_x.max()
    s_x_min = s_x.min()
    s_x_avg = s_x.mean()

    s_y_max = s_y.max()
    s_y_min = s_y.min()
    s_y_avg = s_y.mean()

    g_s_pos, g_s_neg = plt.plot(s_pos_x, s_pos_y, 'g+',
                                s_neg_x, s_neg_y, 'r_')

    # Limit axis.
    if avg:
        plt.xlim((s_x_min, s_x_avg))
        plt.ylim((s_y_min, s_y_avg))
    else:
        plt.xlim((s_x_min, s_x_max))
        plt.ylim((s_y_min, s_y_max))

    plt.xlabel(r'Feature 1')
    plt.ylabel(r'Feature 2')

    return g_s_pos, g_s_neg


def plot_success_per_size(sizes, success):
    """Plot data-size / success graph."""
    g = plt.plot(sizes, success, 'k-')
    plt.xlabel(r'Training Set Size')
    plt.ylabel(r'Success %')

    return g


def plot_w(data, w, color='k'):
    """Plots decision boundary according to vector w."""
    data = np.array(data)

    s_x = data[:, 0]

    s_x_max = s_x.max()
    s_x_min = s_x.min()

    a, b = -w[0]/w[1], -w[-1]/w[1]

    w_x = np.linspace(s_x_min, s_x_max)
    w_y = a*w_x + b

    return plt.plot(w_x, w_y, '%s-' % color, linewidth=2)


def plot_w_legend(gs, sizes):
    """Plots decision boundary vector legend."""
    plt.legend(gs, sizes)


def show():
    plt.show()


def figure(i):
    return plt.figure(i)
