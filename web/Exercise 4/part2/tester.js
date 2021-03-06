'use strict';

/* tester takes two optional arguments:
 * [shouldSkipTests - 'yes'/'no' - default to 'no']
 * [serverTimeoutMilliseconds - default 10000]
 */

var myHttp = require('./myHttp'),
    server = myHttp.createHTTPServer('./public'),
    settings = require('./settings'),
    http = require('http');


// Test counters.
var testsCount = 0,
    failedTestsCount = 0,

    shouldSkipTests = process.argv[2] || 'no',
    serverTimeoutMilliseconds = process.argv[3] || 10000;


function assert(condition, message) {
    if (!condition) {
        console.log('Test %d failed', ++testsCount);
        failedTestsCount++;
        throw message || 'Assertion failed';
    }

    console.log('Test %d succeeded', ++testsCount);
}


// Run server for tests, and pass as a callback our test units.
// We do this so the server is ready for connections when we run the tests.
server.onStart(function () {
    console.log('Starting tests...');

    setTimeout(function () {
        server.stop();
        console.log('Finished running tests.');
        if (failedTestsCount === 0) {
          console.log('all tests passed!');
        } else {
          console.log('%d/%d failed tests.', failedTestsCount, testsCount);
        }
        process.exit(0);
    }, serverTimeoutMilliseconds);

    // Execute tests.
    while (true) {
        // If the second command line argument is yes, we should skip running
        // tests and just launch the server. default: run tests
        if (shouldSkipTests === 'yes') {
            break;
        }

        // Test the server status on init.
        var serverStatus = server.status();
        assert(serverStatus.isStarted === true, 'Server should have been started by now');
        assert(serverStatus.port === settings.TEST_PORT, 'Server is running on the wrong port');
        assert(serverStatus.numOfCurrentRequests === 0, 'Server should report 0 requests on init');
        assert(serverStatus.percentageOfSuccesfulRequests === 100, 'Server should report 100% success rate on init');

        var req = http.request({
            hostname: 'localhost',
            port: settings.TEST_PORT,
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
            port: settings.TEST_PORT,
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
            port: settings.TEST_PORT,
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

        // test for a non existant file
        req = http.request({
            hostname: 'localhost',
            port: settings.TEST_PORT,
            path: '/test.non_existant',
            method: 'GET'
        }, function(res){
            assert(res.httpVersion === '1.1', 'Wrong HTTP version received for /test.non_existant request (Got '+res.httpVersion+')');
            assert(res.statusCode === 404, 'Wrong status code received for /test.non_existant request (Got '+res.statusCode+')');
        });
        req.on('error', function(e) {
            console.log('problem with /test.non_existant request: ' + e.message);
        });
        req.end();

        // try to 'hack' the server by trying to get a file higher than the server root
        req = http.request({
            hostname: 'localhost',
            port: settings.TEST_PORT,
            path: '/../tester.js',
            method: 'GET'
        }, function(res){
            assert(res.httpVersion === '1.1', 'Wrong HTTP version received for /../tester.js request (Got '+res.httpVersion+')');
            assert(res.statusCode === 500, 'Wrong status code received for /../tester.js request (Got '+res.statusCode+')');
        });
        req.on('error', function(e) {
            console.log('problem with /../tester.js request: ' + e.message);
        });
        req.end();

        // test GET parameterized requests
        req = http.request({
            hostname: 'localhost',
            port: settings.TEST_PORT,
            path: '/status/7/45',
            method: 'GET'
        }, function(res){
            assert(res.httpVersion === '1.1', 'Wrong HTTP version received for /status/7/45 request (Got '+res.httpVersion+')');
            assert(res.statusCode === 200, 'Wrong status code received for /status/7/45 request (Got '+res.statusCode+')');
        });
        req.on('error', function(e) {
            console.log('problem with /status/7/45 request: ' + e.message);
        });
        req.end();

        // test POST parameterized requests
        req = http.request({
            hostname: 'localhost',
            port: settings.TEST_PORT,
            path: '/some-resource',
            method: 'POST'
        }, function(res){
            assert(res.httpVersion === '1.1', 'Wrong HTTP version received for /some-resource request (Got '+res.httpVersion+')');
            assert(res.statusCode === 200, 'Wrong status code received for /some-resource request (Got '+res.statusCode+')');
        });
        req.on('error', function(e) {
            console.log('problem with /some-resource request: ' + e.message);
        });
        req.end();

        // test inexistant POST request
        req = http.request({
            hostname: 'localhost',
            port: settings.TEST_PORT,
            path: '/some-nonexistant-resource',
            method: 'POST'
        }, function(res){
            assert(res.httpVersion === '1.1', 'Wrong HTTP version received for /some-inexistant-resource request (Got '+res.httpVersion+')');
            assert(res.statusCode === 404, 'Wrong status code received for /some-inexistant-resource request (Got '+res.statusCode+')');
        });
        req.on('error', function(e) {
            console.log('problem with /some-inexistant-resource request: ' + e.message);
        });
        req.end();

        break;
    }
});


// GET/POST callback tests:
server.get('/status/:id/:phone', function(request, response) {
    assert(request.params.id === '7', 'Given params.id doesn\'t match (Got '+request.params.id+')');
    assert(request.params.phone === '45', 'Given params.phone doesn\'t match (Got '+request.params.phone+')');

    response.status = 200;
    response.end();
});

server.post('/some-resource', function(request, response) {
    assert(true, 'Post callback work');
    response.status = 200;
    response.end();
});


/* Interesting function #1 + #2:
 * #1: returns a html page with the parameter number.
 *     Try: 'http://localhost:3008/test/89' and you'll receive 'Test Number: 89' in the page.
 * #2: returns a json response with the param number.
 *     Try: 'http://[address]:[port]/interesting.html'
 */
server.get('/test/:number', function(request, response) {
    var number = request.params.number;
    response.status = 200;

    if (number === 'one') {
        response.end('<html><body>Test Number: ' + request.params.number + '</body></html>');
    } else if (number === 'two') {
        response.headers['Content-Type'] = 'application/json';
        response.end(JSON.stringify( { 'test_number': number } ));
    } else {
        response.status = 404;
        response.end();
    }
});


/* Interesting function #3:
 * Receives a json object, and echoes it back as a response.
 * Try: 'http://[address]:[port]/interesting.html'
 */
server.post('/test/:number', function(request, response) {
    var number = request.params.number;

    // TODO AJAX parameters are in response object, find out how to fetch them.
    if (number === 'three') {
        response.headers['Content-Type'] = 'application/json';
        response.end(JSON.stringify(request.body));
    } else {
        response.status = 404;
        response.end();
    }
});


// Verbose prints.
console.log('Skipping tests: ' + shouldSkipTests);
console.log('Server alive for ' + (serverTimeoutMilliseconds / 1000) + ' seconds.\n');


// Start server and tests.
server.start(settings.TEST_PORT);
