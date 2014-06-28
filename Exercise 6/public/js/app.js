'use strict';

var app = app || {},
    ENTER_KEY = 13,
    SOCKETIO_ADDRESS = 'http://10.0.0.13:4000';

$(function () {
    // Kick things off by creating the `App`
    new app.AppView();
});
