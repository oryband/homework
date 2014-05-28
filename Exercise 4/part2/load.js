'use strict';

/*
 * Tester takes two optional arguments:
 * [amountOfConcurrentRequests - amount of requests to execute - default 1000]
 * [serverTimeoutMilliseconds - default 10000]
 */

var myHttp = require('./myHttp'),
    server = myHttp.createHTTPServer('./public'),
    settings = require('./settings'),
    http = require('http');

var amountOfConcurrentRequests = process.argv[2] || 1000,
    serverTimeoutMilliseconds = process.argv[3] || 10000;

// Run server for tests, and pass as a callback our test units.
// We do this so the server is ready for connections when we run the tests.
server.start(settings.TEST_PORT, function () {
    setTimeout(function () {
        server.stop();
        console.log('Finished load test.');
        process.exit(0);
    }, serverTimeoutMilliseconds);

    // LETS CRASH THE SERVER!
    var req;
    for (var i = 0; i < amountOfConcurrentRequests; ++i) {
        (function () {
            var currentRequestID = i;

            console.log('Sent request %d', currentRequestID);

            req = http.request({
                hostname: 'localhost',
                port: settings.TEST_PORT,
                path: '/status',
                method: 'GET'
            }, function() {
                console.log('Got response %d', currentRequestID);
            });

            req.end();
        })();
    }
});
