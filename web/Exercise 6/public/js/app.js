'use strict';

var app = app || {},
    ENTER_KEY = 13,
    SOCKETIO_ADDRESS = 'http://' + window.location.hostname + ':4000',
    RETRY_DELAY = 60000;  // In ms.

$(function () {
    Backbone.emulateHTTP = true;

    // Kick things off by creating the `App`
    new app.AppView();
});
