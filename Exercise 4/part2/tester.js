'use strict';

function assert(condition, message) {
  if (!condition) {
    throw message || 'Assertion failed';
  }
}

var myHttp = require('./myHttp'),
    server = myHttp.createStaticHttpServer('./public');

var http = require('http');

var TEST_PORT = 3008;

server.start(TEST_PORT);
setTimeout(function () {
    server.stop();
}, 5000);

// print the server status on init

var serverStatus = server.status();
assert(serverStatus.isStarted === true, 'Server should have been started by now');
assert(serverStatus.port === TEST_PORT, 'Server is running on the wrong port');
assert(serverStatus.numOfCurrentRequests === 0, 'Server should report 0 requests on init');
assert(serverStatus.percentageOfSuccesfulRequests === 100, 'Server should report 100% success rate on init');

var req;

req = http.request({
    hostname: 'localhost',
    port: TEST_PORT,
    path: '/status',
    method: 'GET'
  }, function(res){
  assert(res.httpVersion === '1.1', 'Wrong HTTP version received for /status request (Got '+res.httpVersion+')');
  assert(res.statusCode === 200, 'Wrong status code received for /status request (Got '+res.statusCode+')');
  assert(res.headers['content-type'] === 'text/html', '/status request should return text/html content (Got '+res.headers['content-type']+')');

  var serverStatus = server.status();
  assert(serverStatus.numOfCurrentRequests === 1, 'Server should report 1 requests after /status');
  assert(serverStatus.percentageOfSuccesfulRequests === 100, 'Server should report 100% success rate after a successful status request');
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

  var serverStatus = server.status();
  assert(serverStatus.numOfCurrentRequests === 2, 'Server should report 2 requests after bad method request');
  assert(serverStatus.percentageOfSuccesfulRequests === 50, 'Server should report 50% success rate after a successful and then a bad request');
});
req.on('error', function(e) {
  console.log('problem with PUT /status BAD request: ' + e.message);
});
req.end();
