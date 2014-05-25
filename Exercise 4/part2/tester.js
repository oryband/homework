'use strict';

var http = require('./myHttp'),
    server = http.createStaticHttpServer('./public');

server.start(3008);
setTimeout(function () {
    server.stop();
}, 10000);
