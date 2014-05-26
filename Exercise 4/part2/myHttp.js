var events = require('events'),
    fs = require('fs'),
    net = require('net'),
    util = require('util'),

    settings = require('./settings');


// Immutable HTTP Request object.
var HttpRequest = function (reqVersion, reqMethod, reqUri, reqResource, reqResPath, reqHeaders, reqBody) {
    'use strict';

    var version = reqVersion,
        method = reqMethod,
        uri = reqUri,
        resource = reqResource,
        resPath = reqResPath,
        headers = reqHeaders,
        body = reqBody;

    return {
        get version() { return version; },
        get method() { return method; },
        get uri() { return uri; },
        get resource() { return resource; },
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
        get resPath() { return resPath; },
        get headers() { return headers; },
        get body() { return body; }
    };
};


// HTTP Response object.
var HttpResponse = function (resSocket, resVersion, resStatus, resHeaders, resBody) {
    'use strict';

    // TODO add error checks, out of exercise's scope.
    var version = resVersion || '1.1',
        status = resStatus,
        body = resBody || '',
        headers = resHeaders || {'Content-Length': 0},
        socket = resSocket;

    return {
        get version() { return version; },
        set version(x) { version = x; },

        get status() { return status; },
        set status(x) { status = x; },

        get body() { return body; },
        set body(x) { body = x; },

        get headers() { return headers; },
        set headers(x) { headers = x; },

        write: function (str) {
          body += str;
        },

        end: function (str) {
          body += str || '';
          this.headers['Content-Length'] = body.length;
          socket.write(this.toString());
        },

        // Build response string and return it.
        toString: function() {
            return [
                'HTTP/' + version + ' ' + status + ' ' + settings.HTTP_STATUS_CODES[status],
                (function () {
                  var processedHeaders = [];
                  for (var key in headers) {
                      if (headers.hasOwnProperty(key)) {
                          processedHeaders.push(key + ':' + headers[key]);
                      }
                  }
                  return processedHeaders;
                })().join('\r\n'),
                '',
                body
            ].join('\r\n');
        }
    };
};


// Parses a HTTP request string, and returns an HttpRequest object on success or {} otherwise.
function parseRequest(req) {
    'use strict';

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

        // Headers and body are separated by CRLF. This will result in an
        // empty line. If detected, break the headers parsing and start parsing
        // the body lines.
        if (line === '') {
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
    i += 2;
    var body = lines.splice(i, lines.length).join('\r\n');

    // Build response.
    var httpRequest = new HttpRequest(version, method, uri, resource, resPath, headers, body);

    return httpRequest;
}


// Server class to be instantiated and returned upon calling createHTTPServer().
var Server = function (rootFolder) {
    'use strict';
    var that = this;  // Reference for use in inner closures.

    var serverStarted = false,
        startDate = null,
        serverPort = null,
        serverCurrentRequests = 0,
        serverTotalRequests = 0,
        serverSuccessfulRequests = 0,
        shouldShutdownServer = false,
        getCallbacks = [],
        postCallbacks = [];

    // Return a server status object with the above fields.
    function status () {
        return {
            isStarted: serverStarted,
            startedDate: startDate,
            port: serverPort,
            numOfCurrentRequests: serverCurrentRequests,
            percentageOfSuccesfulRequests: serverTotalRequests === 0 ? 100 : Math.floor(serverSuccessfulRequests * 100 / serverTotalRequests)
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
            lastRequests.unshift(currentTime);  // Remember request time.

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
    function processStatus(socket) {
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
        new HttpResponse(
            socket,
            1.1,
            200,
            {'Content-Type': 'text/html'}
        ).end(body);
    }


    // Handle annoying favicon requests.
    function processFavicon(socket) {
        new HttpResponse(socket, 1.1, 404).end();
    }


    // Close socket and update current request counter.
    function closeConnection(socket) {
        serverCurrentRequests--;
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


    // Build HTTP response and return file requested as resource.
    function processValidRequest(socket, request, rootFolder) {
        // TODO Make sure people don't use ../ and access restricted files.
        var path = rootFolder + request.resPath + request.resource;

        fs.stat(path, function (err, stats) {
            if (err) {
                // Errors probably means file doesn't exist,
                // or the client is trying to be smart.
                // In any case - respond with 404.
                console.error('stat failed: %s', err.message);
                new HttpResponse(socket, 1.1, 404).end();
                return;
            } else if (stats.isDirectory()) {
                console.error('resource "%s" a directory', path);
                new HttpResponse(socket, 1.1, 403).end();
                return;
            }


            var response = new HttpResponse(socket, 1.1, 200);
            response.headers['Content-Type'] = request.contentType;

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
        // Used to remember time of last requests, to defend against (psuedo-)DoS attacks.
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
            if (shouldShutdownServer) { return; }

            // Check if this socket sent too many requests.
            if (!checkConnectionCap(lastRequests)) { return; }

            // If not, start processing.
            serverTotalRequests++;
            serverCurrentRequests++;

            var request = parseRequest(req.toString());

            // Handle bad requests.
            if (!checkBadRequest(socket, request)) { return; }

            // If we got this far, the request is successful (but needs to be further parsed)
            serverSuccessfulRequests++;

            if (request.method === 'GET') {
              that.emit('get', request);
            } else if (request.method === 'POST') {
              that.emit('post', request);
            }

            // Process request.
            if (request.resource === '/status') {
                processStatus(socket);
            } else if (request.resource === '/favicon.ico') {
                processFavicon(socket);
            } else {
                processValidRequest(socket, request, rootFolder);
            }

            // Check if we need to close the connection.
            if (checkCloseConnection(socket, request)) {
                // Update current request counter.
                serverCurrentRequests--;
            }
        });
    });


    // Public.
    return {
        start: function (port) {
            netServer.listen(port, function () {
                serverStarted = true;
                startDate = new Date().toLocaleDateString();
                serverPort = port;

                // Fire 'server started' event.
                that.emit('start');

                that.on('get', function(request) {
                  // find a getCallbacks that matches the request.resPath
                  for (var i in getCallbacks) {
                    var obj = getCallbacks[i];
                    // TODO: match between resource to the actual resPath
                    if (obj.resource === request.resPath) {
                      obj.callback(request, new HttpResponse());
                      return;
                    }
                  }

                  // execute the default static behavior
                });

                that.on('post', function(request) {
                  // find a postCallbacks that matches the request.resPath
                  for (var i in postCallbacks) {
                    var obj = postCallbacks[i];
                    // TODO: match between resource to the actual resPath
                    if (obj.resource === request.resPath) {
                      obj.callback(request, new HttpResponse());
                      return;
                    }
                  }

                  // execute the default static behavior
                });
            });
        },

        stop: function stopServer () {
            shouldShutdownServer = true;

            if (serverCurrentRequests === 0) {
                console.log('Shutting down server...');
                netServer.close();
                return;
            }

            // retry stop in a second
            setInterval(stopServer, 1000);
        },

        status: status,

        onStart: function (callback) {
            that.on('start', callback);
        },

        get: function(resource, callback) {
            getCallbacks.push({'resource': resource, 'callback': callback});
        },

        post: function(resource, callback) {
            postCallbacks.push({'resource': resource, 'callback': callback});
        }
    };
};


// Server implements EventEmitter interface.
util.inherits(Server, events.EventEmitter);


exports.createHTTPServer = function (rootFolder) {
    'use strict';
    return new Server(rootFolder);
};
