var net = require('net'),
    fs = require('fs'),
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
var HttpResponse = function (resVersion, resStatusCode, resResponseHeader, resBody) {
    'use strict';

    // TODO add error checks, out of exercise's scope.
    var version = resVersion,
        statusCode = resStatusCode,
        responseHeader = resResponseHeader || ['Content-Length: 0'],
        body = resBody || '';

    return {
        get version() { return version; },
        get statusCode() { return statusCode; },
        get responseHeader() { return responseHeader; },
        get body() { return body; },

        set version(x) { version = x; },
        set statusCode(x) { statusCode = x; },
        set responseHeader(x) { responseHeader = x; },
        set body(x) { body = x; },

        // Build response string and return it.
        toString: function() {
            return [
                'HTTP/' + version + ' ' + statusCode + ' ' + settings.HTTP_STATUS_CODES[statusCode],
                responseHeader.join('\r\n'),
                '',
                body
            ].join('\r\n');
        }
    };
};


// Parses a HTTP request string, and returns an HttpRequest object on success,
// returns {} otherwise.
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
    resPath = uri.substring(0, uri.lastIndexOf('/') -1);

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


var Server = function (rootFolder) {
    'use strict';

    var serverStarted = false,
        startDate = null,
        serverPort = null,
        serverCurrentRequests = 0,
        serverTotalRequests = 0,
        serverSuccessfulRequests = 0;

    // Return a server status object with the above fields.
    var status = function () {
        return {
            isStarted: serverStarted,
            startedDate: startDate,
            port: serverPort,
            numOfCurrentRequests: serverCurrentRequests,
            percentageOfSuccesfulRequests: serverTotalRequests === 0 ? 100 : Math.floor(serverSuccessfulRequests * 100 / serverTotalRequests)
        };
    };

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
            // Allow a capped amount of requests per connection.
            var currentTime = new Date().getTime();
            if (lastRequests.length >= settings.MAX_REQUESTS_PER_CONNECTION &&
                currentTime - lastRequests[settings.MAX_REQUESTS_PER_CONNECTION -1] < settings.REQUESTS_TIME_THRESHOLD_IN_SEC * 1000) {

                // Return an error if too many requests happenned too quickly.
                socket.write(new HttpResponse(1.1, 500).toString());
                return;
            } else {
                // Forget oldest request time (we remember only a capped amount).
                if (lastRequests.length >= settings.MAX_REQUESTS_PER_CONNECTION) {
                    lastRequests.pop();
                }

                // If this request is approved, remember its time for future tests.
                lastRequests.unshift(currentTime);  // Remember request time.
            }

            serverTotalRequests++;
            serverCurrentRequests++;

            var request = parseRequest(req.toString());

            // Return Bad Request HTTP response on bad requests.
            if (!request) {
                socket.write(new HttpResponse(1.1, 400).toString());
                return;
            } else if (request.method !== 'GET' && request.method !== 'POST') {
                socket.write(new HttpResponse(1.1, 405).toString());
                return;
            }

            // If we got this far, the request is successful (but needs to be further parsed)
            serverSuccessfulRequests++;

            if (request.resource === '/status') {
                // Route '/status' to status page.
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
                socket.write(new HttpResponse(
                    1.1,
                    200,
                    [
                        'Content-Type: text/html',
                        'Content-Length: ' + body.length
                    ],
                    body
                ).toString());

            } else if (request.resource === '/favicon.ico') {
                // Handle annoying favicon requests.
                socket.write(new HttpResponse(1.1, 404).toString());

            } else {
                // TODO Make sure people don't use ../ and access restricted files.
                // Build HTTP response and return file requested as resource.
                var response = new HttpResponse(1.1, 200),
                    path = rootFolder + request.resource;

                fs.stat(path, function (err, stats) {
                    if (err) {
                        // Errors probably means file doesn't exist,
                        // or the client is trying to be smart.
                        // In any case - respond with 404.
                        console.error('stat failed: %s', err.message);
                        socket.write(new HttpResponse(1.1, 404).toString());
                        return;
                    }

                    response.responseHeader = [
                        'Content-Type: ' + request.contentType,
                        'Content-Length: ' + stats.size
                    ];

                    // Write HTTP headers first.
                    socket.write(response.toString());

                    // Now stream the file: we are streaming it instead of
                    // fs.readFile() because readFile will load to memory
                    // first before serving, which might cause a memory overflow.
                    var stream = fs.createReadStream(path);
                    stream.pipe(socket, {'end': false});
                });
            }

            // Close connection if protocol is 1.0 and missing keep-alive header.
            var closeConnection = function () {
                serverCurrentRequests--;
                socket.end();
            };

            if (request.headers.Connection === 'close' ||
                (request.version === '1.0' && request.headers.Connection !== 'Keep-Alive')
            ) {
              closeConnection();

            } else {
                // Set a timeout based on the config value,
                // Because we shouldn't immediatly close the connection.
                socket.setTimeout(settings.LAST_REQUEST_TIMEOUT_SEC * 1000, function () {
                    closeConnection();
                });
            }
        });
    });


    // Public methods.
    return {
        start: function (port, callback) {
            netServer.listen(port, function () {
                serverStarted = true;
                startDate = new Date().toLocaleDateString();
                serverPort = port;

                if (callback) { callback(); }
            });
        },

        stop: function () {
            netServer.close();
        },

        status: status
    };
};


exports.createStaticHttpServer = function (rootFolder) {
    'use strict';
    return new Server(rootFolder);
};
