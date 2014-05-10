#!/usr/bin/env python


def tag(w, s):
    """Classifies a sample according to weights,
    by calculating if wTx >= 0 (dot product).
    """
    dp = 0  # Dot product.
    for wi, si in zip(w[:-1], s):
        dp += wi*si

    dp += w[-1]  # Add bias.

    return 1 if dp >= 0 else -1


def train(data, max_iter=100, r=0.1):
    """Trains perceptron on data, and returns a w in R^n vector.

    max_iter: Maximum # of iterations.
    r: Learning rate.
    """
    # Number of features (N form R^N). The last value is the desired label,
    # so we omit it.
    dim = len(data[0]) - 1
    w = [0] * (dim+1)  # Weight vector. We add an extra dimension for the bias.

    for j in xrange(max_iter):  # Maximum of 100 iterations.
        err = 0
        for x in data:
            s, d = x[:-1], x[-1]  # Sample / desired label.
            if tag(w, s) != d:  # If we labeled wrong.
                for i in xrange(dim):  # Update weights.
                    w[i] += r * d * s[i]

                w[-1] += r * d  # Update bias.

                err += 1

        if err == 0:
            break

    print 'Iterations/Bound: %d/%d' % (j+1, max_iter)

    return w


def test(data, w):
    """Tests perceptron w on data. Returns error/total percentage."""
    errors = 0

    for x in data:
        s, d = x[:-1], x[-1]
        if tag(w, s) != d:
            errors += 1

    percentage = 100.0 * errors / len(data)
    return percentage
