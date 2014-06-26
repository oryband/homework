'use strict';

var app = app || {},
    SOCKETIO_ADDRESS = 'http://localhost:4000';

$(function () {
    // Kick things off by creating the `App`
    new app.AppView();
});
