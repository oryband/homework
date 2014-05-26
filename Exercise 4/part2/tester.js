'use strict';

/*
 * tester takes two optional arguments:
 * [shouldSkipTests - 'yes'/'no' - default to 'no'] 
 * [serverTimeoutMilliseconds - default 10000]
 */

// self explanatory. I hope...
function assert(condition, message) {
    if (!condition) {
        throw message || 'Assertion failed';
    }
}

var myHttp = require('./myHttp'),
    server = myHttp.createStaticHttpServer('./public');

var http = require('http');

var TEST_PORT = 3008;

// run server for tests, and pass as a callback our test units.
// we do this so the server is ready for connections when we run the tests.
server.start(TEST_PORT, function () {
    setTimeout(function () {
        server.stop();
        console.log('Finished running tests.');
        process.exit(0);
    }, process.argv[3] || 10000);

    // Execute tests.
    while (true) {
        // If the second command line argument is yes, we should skip running
        // tests and just launch the server. default: run tests
        if ((process.argv[2] || 'no') === 'yes') {
            break;
        }

        // Test the server status on init.
        var serverStatus = server.status();
        assert(serverStatus.isStarted === true, 'Server should have been started by now');
        assert(serverStatus.port === TEST_PORT, 'Server is running on the wrong port');
        assert(serverStatus.numOfCurrentRequests === 0, 'Server should report 0 requests on init');
        assert(serverStatus.percentageOfSuccesfulRequests === 100, 'Server should report 100% success rate on init');

        var req = http.request({
            hostname: 'localhost',
            port: TEST_PORT,
            path: '/status',
            method: 'GET'
        }, function(res){
            assert(res.httpVersion === '1.1', 'Wrong HTTP version received for /status request (Got '+res.httpVersion+')');
            assert(res.statusCode === 200, 'Wrong status code received for /status request (Got '+res.statusCode+')');
            assert(res.headers['content-type'] === 'text/html', '/status request should return text/html content (Got '+res.headers['content-type']+')');
        });

        req.on('error', function(e) {
            console.log('problem with /status request: ' + e.message);
        });

        req.end();

        req = http.request({
            hostname: 'localhost',
            port: TEST_PORT,
            path: '/status',
            method: 'PUT'
        }, function(res){
            assert(res.httpVersion === '1.1', 'Wrong HTTP version received for /status request (Got '+res.httpVersion+')');
            assert(res.statusCode === 405, 'Wrong status code received for a method not allowed request (Got '+res.statusCode+')');
        });

        req.on('error', function(e) {
            console.log('problem with PUT /status BAD request: ' + e.message);
        });

        req.end();

        req = http.request({
            hostname: 'localhost',
            port: TEST_PORT,
            path: '/test.jpg',
            method: 'GET'
        }, function(res){
            assert(res.httpVersion === '1.1', 'Wrong HTTP version received for /test.jpg request (Got '+res.httpVersion+')');
            assert(res.statusCode === 200, 'Wrong status code received for /test.jpg request (Got '+res.statusCode+')');
            assert(res.headers['content-type'] === 'image/jpeg', '/test.jpg request should return text/jpeg content (Got '+res.headers['content-type']+')');
        });

        req.on('error', function(e) {
            console.log('problem with /test.jpg request: ' + e.message);
        });

        req.end();

        break;
    }
});
