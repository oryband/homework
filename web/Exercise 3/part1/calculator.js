"use strict";

var Calculator = function () {
    // Private.
    var screen = 0;

    this.getScreen = function () {
        return screen;
    };

    // We assume valid value was given.
    this.add = function (value) {
        screen += value;
    };

    this.multiply = function (value) {
        screen *= value;
    };

    this.clear = function () {
        screen = 0;
    };
};
