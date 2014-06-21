'use strict';

// 3rd-party modules.
var events = require('events'),
    fs = require('fs'),
    net = require('net'),
    util = require('util'),
    _ = require('underscore')._;

// User created modules.
var settings = require('./settings');


// Immutable HTTP Request object.
var HttpRequest = function (reqVersion, reqMethod, reqUri, reqResource, reqResPath, reqHeaders, reqBody) {
    var _version = reqVersion,
        _method = reqMethod,
        _uri = reqUri,
        _resource = reqResource,
        _resPath = reqResPath,
        _headers = reqHeaders,
        _body = reqBody,
        _params = {};

    return {
        get version() { return _version; },
        get method() { return _method; },
        get uri() { return _uri; },
        get resource() { return _resource; },
        get contentType() {
            // Get resource extension.
            switch (this.resource.substring(this.resource.lastIndexOf('.') +1)) {
                case 'css': return 'text/css';
                case 'gif': return 'image/gif';
                case 'html': return 'text/html';
                case 'js': return 'application/javascript';
                case 'json': return 'application/json';
                case 'jpg': return 'image/jpeg';
                default: return 'text/plain';
            }
        },
        get resPath() { return _resPath; },
        get headers() { return _headers; },
        get body() { return _body; },
        get params() { return _params; },
        set params(newParams) { _params = newParams; }
    };
};


// HTTP Response object.
var HttpResponse = function (resSocket, resVersion, resStatus, resHeaders, resBody) {
    var _version = resVersion || '1.1',
        _status = resStatus,
        _body = resBody || '',
        _headers = resHeaders || {},
        _socket = resSocket;

    return {
        get version() { return _version; },
        set version(x) { _version = x; },

        get status() { return _status; },
        set status(x) { _status = x; },

        get body() { return _body; },
        set body(x) { _body = x; },

        get headers() { return _headers; },
        set headers(x) { _headers = x; },

        // Append data to body
        write: function (str) {
            _body += str;
        },

        // Append data to body and write response to socket.
        end: function (str) {
            _body += str || '';

            // Set content-length automatically if not already set by user.
            this.headers['Content-Length'] = this.headers['Content-Length'] || _body.length;

            _socket.write(this.toString());
        },

        // Return response string.
        toString: function() {
            return [
                'HTTP/' + _version + ' ' + _status + ' ' + settings.HTTP_STATUS_CODES[_status],
                (function () {
                    var processedHeaders = [];
                    for (var key in _headers) {
                        if (_headers.hasOwnProperty(key)) {
                            processedHeaders.push(key + ':' + _headers[key]);
                        }
                    }
                    return processedHeaders;
                })().join('\r\n'),
                '',
                _body
            ].join('\r\n');
        }
    };
};


// Parses a HTTP request string, and returns an HttpRequest object on success or {} otherwise.
function parseRequest(req) {
    if (!req) {
        console.error('request is null.');
        return {};
    }

    // Parse request line.
    var lines = req.split('\r\n'),
    reqLine = lines[0].split(' ');

    if (reqLine.length !== 3) {
        console.error('Request line not valid.');
        return {};
    }

    // Parse request method.
    var method = reqLine[0],
    uri = reqLine[1],
    version = reqLine[2];

    if (method !== 'GET' && method !== 'POST') {
        console.error('Method "%s" not GET or POST.', method);
        return {};
    }

    // Parse request URI.
    if (! /((ftp|http|https):\/\/(\w+:{0,1}\w*@)?(\S+)(:[0-9]+)?)?(\/|\/([\w#!:.?+=&%@!\-\/]))?/.test(uri, 'i')) {
        console.error('Bad URI "%s"', uri);
        return {};
    }

    // Parse HTTP version.
    if (version !== 'HTTP/1.0' && version !== 'HTTP/1.1') {
        console.error('Bad version.');
        return {};
    }

    // Parse path and resource.
    var resource = uri.substring(uri.lastIndexOf('/'), uri.length),
    resPath = uri.substring(0, uri.lastIndexOf('/'));

    // Parse headers.
    var i,
    headers = {};
    for (i = 1; i < lines.length; ++i) {
        var line = lines[i];

        // Headers and body are separated by CRLF. This will result in an empty line.
        // If detected, break the headers parsing and start parsing the body lines.
        if (line === '') {
            i++;  // Index points at last header line, increase to point at first body line.
            break;
        }

        var parts = line.split(': ');
        if (parts.length !== 2) {
            console.error('Invalid header received "%s"', line);
            return {};
        }

        var key = parts[0],
        value = parts[1];

        headers[key] = value;
    }

    // Parse body.
    var body = lines.splice(i, lines.length).join('\r\n');

    // Build response.
    var httpRequest = new HttpRequest(version, method, uri, resource, resPath, headers, body);

    return httpRequest;
}


// Server class to be instantiated and returned upon calling createHTTPServer().
var Server = function (rootFolder) {
    var server = this;  // Reference for use in inner closures.

    var _serverStarted = false,
        _startDate = null,
        _serverPort = null,
        _serverCurrentRequests = 0,
        _serverTotalRequests = 0,
        _serverSuccessfulRequests = 0,
        _shouldShutdownServer = false,
        _getCallbacks = [],
        _postCallbacks = [];

    // Return a server status object with the above fields.
    function status () {
        return {
            isStarted: _serverStarted,
            startedDate: _startDate,
            port: _serverPort,
            numOfCurrentRequests: _serverCurrentRequests,
            percentageOfSuccesfulRequests: _serverTotalRequests === 0 ? 100 : Math.floor(_serverSuccessfulRequests * 100 / _serverTotalRequests)
        };
    }


    // Return true if we should process the connection, false other wise.
    function checkConnectionCap(lastRequests) {
        // Allow only a capped amount of requests per connection.
        var currentTime = new Date().getTime();
        if (lastRequests.length >= settings.MAX_REQUESTS_PER_CONNECTION &&
            currentTime - lastRequests[settings.MAX_REQUESTS_PER_CONNECTION -1] < settings.REQUESTS_TIME_THRESHOLD_IN_SEC * 1000) {

            // don't respond to this request!
            // the user is DoS-ing and that's what he wants us to do exactly.
            return false;
        } else {
            // Forget oldest request time (we remember only a capped amount).
            if (lastRequests.length >= settings.MAX_REQUESTS_PER_CONNECTION) {
                lastRequests.pop();
            }

            // If this request is approved, remember its time for future tests.
            lastRequests.unshift(currentTime);

            return true;
        }
    }


    // Return Bad Request HTTP response on bad requests.
    function checkBadRequest(socket, request) {
        if (!request) {
            // Handle invalid requests.
            new HttpResponse(socket, 1.1, 400).end();
            return false;
        } else if (request.method !== 'GET' && request.method !== 'POST') {
            // Only GET or POST requests are allowed.
            new HttpResponse(socket, 1.1, 405).end();
            return false;
        } else {
            return true;
        }
    }


    // Route '/status' to status page.
    function processStatus(response) {
        var stat = status(),
            body = '<html><h1>Status</h1><ul>';

        // Append status elements.
        for (var key in stat) {
            if (stat.hasOwnProperty(key)) {
                body += '<li><b>' + key + '</b>: ' + stat[key] + '</li>';
            }
        }

        // Finish building status page.
        body += '</ul></html>';

        // Return response.
        response.status = 200;
        response.headers['Content-Type'] = 'text/html';
        response.end(body);
    }


    // Handle annoying favicon requests.
    function processFavicon(response) {
        response.status = 404;
        response.end();
    }


    // Close socket and update current request counter.
    function closeConnection(socket) {
        _serverCurrentRequests--;
        socket.end();
    }


    // Close connection if protocol is 1.0 and missing keep-alive header.
    function checkCloseConnection(socket, request) {
        if (request.headers.Connection === 'close' ||
            (request.version === '1.0' && request.headers.Connection !== 'Keep-Alive')) {

            closeConnection(socket);
        } else {
            // Set a timeout based on the config value,
            // Because we shouldn't immediatly close the connection.
            socket.setTimeout(settings.LAST_REQUEST_TIMEOUT_SEC * 1000, function () {
                closeConnection(socket);
            });
        }
    }


    // Execute a user-defined GET/POST request callback that matches
    // the request's resource path with the given parameters.
    function processCallback(socket, request, type) {
        var callbacks = type === 'get' ? _getCallbacks : _postCallbacks;
        for (var i in callbacks) {
            if (callbacks.hasOwnProperty(i)) {
                var obj = callbacks[i],
                    resource = obj.resource;

                // Create a regex from the parameterized resource.
                // ie: /status/:id/:phone => /^\/status\/([^\s/]+)\/([^\s/]+)$/
                var paramRegex = /:[^\s/$]+/g,
                    matchRegex = new RegExp(resource.replace(paramRegex, '([^\\s/]+)'));

                // Check for a match between the regex and the requested resource.
                var result = request.uri.match(matchRegex);
                if (result) {
                    // Extract param names.
                    var paramNames = resource.match(paramRegex) || [];

                    // Construct the param object.
                    var params = {};
                    for (var j = 0; j < paramNames.length; ++j) {
                        params[paramNames[j].substring(1)] = result[j+1];
                    }

                    // Push to request object and return.
                    request.params = params;
                    obj.callback(request, new HttpResponse(socket));
                    return;
                }
            }
        }

        if (type === 'get') {
            // If no callback was found, try serving the static file.
            processValidRequest(socket, request, rootFolder);
        } else { // POST
            // Respond with 'not found' if no resource matched from the callback list,
            var response = new HttpResponse(socket);
            response.status = 404;
            response.end();
        }
    }


    // Build HTTP response and return file requested as resource.
    function processValidRequest(socket, request, rootFolder) {
        var response = new HttpResponse(socket),
            path = rootFolder + request.resPath + request.resource;

        // Avoid higher-level directory access attempts in resource.
        if (path.indexOf('../') !== -1) {
          console.error('tried to access illegal path "%s"', path);
          response.status = 500;
          response.end();
          return;
        }

        fs.stat(path, function (err, stats) {
            if (err) {
                // Errors probably means file doesn't exist, or the client is trying to be smart.
                // In any case - respond with 404.
                console.error('stat failed: %s', err.message);
                response.status = 404;
                response.end();
                return;
            } else if (stats.isDirectory()) {
                console.error('resource "%s" a directory', path);
                response.status = 403;
                response.end();
                return;
            }

            response.status = 200;
            response.headers['Content-Type'] = request.contentType;
            response.headers['Content-Length'] = stats.size;

            // Write HTTP headers first.
            response.end();

            // Now stream the file: we are streaming it instead of
            // fs.readFile() because readFile will load to memory
            // first before serving, which might cause a memory overflow.
            var stream = fs.createReadStream(path);
            stream.pipe(socket, {'end': false});
        });
    }


    // Handle new connections.
    var netServer = net.createServer(function (socket) {
        // Used to remember time of last requests, to protect against (psuedo-)DoS attacks.
        var lastRequests = [];

        // Handle Errors.
        socket.on('error', function (err) {
            console.error('Socket error: %s', err.message);
            console.error(err.stack);
        });

        // Handle incoming requests for this socket.
        socket.on('data', function (req) {
            // Don't process request if server is in process of shutting down,
            // and don't accept anymore requests
            if (_shouldShutdownServer) { return; }

            // Check if this socket sent too many requests.
            if (!checkConnectionCap(lastRequests)) { return; }

            // If not, start processing.
            _serverTotalRequests++;
            _serverCurrentRequests++;

            var request = parseRequest(req.toString());

            // Handle bad requests.
            if (!checkBadRequest(socket, request)) { return; }

            // If we got this far, the request is successful (but needs to be further parsed).
            _serverSuccessfulRequests++;

            // Emit request type event, so registered callbacks would be executed.
            if (request.method === 'GET') {
              server.emit('get', socket, request, 'get');
            } else if (request.method === 'POST') {
              server.emit('post', socket, request, 'post');
            }
        });
    });


    // Public.
    return {
        // Server start.
        start: function (port) {
            netServer.listen(port, function () {
                _serverStarted = true;
                _startDate = new Date().toLocaleDateString();
                _serverPort = port;

                // Fire 'server started' event.
                server.emit('start');

                // Process user callbacks (if defined).
                server.on('get', processCallback);
                server.on('post', processCallback);

                // Close connection if necessary.
                server.on('get', function(socket, request) {
                    checkCloseConnection(socket, request);
                });
                server.on('post', function(socket, request) {
                    checkCloseConnection(socket, request);
                });

                // Handle status requests.
                this.get('/status', function (request, response) {
                    processStatus(response);
                });

                // Handle annoying favicon requests.
                this.get('/favicon.ico', function (request, response) {
                    processFavicon(response);
                });
            }.bind(this));
        },

        // Server stop.
        stop: function stopServer () {
            _shouldShutdownServer = true;

            if (_serverCurrentRequests === 0) {
                console.log('Shutting down server...');
                netServer.close();
                return;
            }

            // Retry stop in a second
            setInterval(stopServer, 1000);
        },

        // Return status object.
        status: status,

        // Register on-start callback.
        onStart: function (callback) {
            server.on('start', callback);
        },

        // Register GET request callback.
        get: function(resource, callback) {
            _getCallbacks.push({'resource': resource, 'callback': callback});
        },

        // Register POST request callback.
        post: function(resource, callback) {
            _postCallbacks.push({'resource': resource, 'callback': callback});
        }
    };
};


// Server extends EventEmitter.
util.inherits(Server, events.EventEmitter);


// Module exports: Function returns a new Server object (defined above).
exports.createHTTPServer = function (rootFolder) {
    return new Server(rootFolder);
};


// Parse request body as POST, and return an object of { key: value }.
// NOTE we assume here that request.body is POST.
exports.parsePostBody = function (request) {
    return _.object(decodeURIComponent(request.body.replace(/\+/g, ' '))
                    .split('&')
                    .map(function(el) { return el.split('='); }));
};
