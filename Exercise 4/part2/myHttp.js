var net = require('net');

exports.createStaticHttpServer = function (rootFolder) {
    'use strict';

    return net.createServer({}, function(c) {
    });
};
