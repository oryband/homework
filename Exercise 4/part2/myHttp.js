var net = require('net'),
    settings = require('./settings');


// Immutable HTTP Request object.
var HttpRequest = function (reqMethod, reqUri, reqResource, reqResPath, reqHeaders, reqBody) {
    'use strict';

    var method = reqMethod,
        uri = reqUri,
        resource = reqResource,
        resPath = reqResPath,
        headers = reqHeaders,
        body = reqBody;

    return {
        get method() { return method; },
        get uri() { return uri; },
        get resource() { return resource; },
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
    var httpRequest = new HttpRequest(method, uri, resource, resPath, headers, body);

    return httpRequest;
}


var Server = function (rootFolder) {
    'use strict';

    var serverStarted = false,
        startDate = null,
        serverPort = null,
        serverCurrentRequests = 0,  // TODO: update on connection open & close.
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


    var netServer = net.createServer(function (socket) {
        socket.on('data', function (request) {
            socket.setTimeout(settings.LAST_REQUEST_TIMEOUT_SEC * 1000, function () {
                socket.end();
            });

            var httpRequest = parseRequest(request.toString());

            // Return Bad Request HTTP response on bad requests.
            if (!httpRequest) {
                socket.write(new HttpResponse(1.1, 400).toString());
                return;
            }

            if (httpRequest.method !== 'GET' && httpRequest.method !== 'POST') {
                socket.write(new HttpResponse(1.1, 405).toString());
                return;
            }

            // Route '/status' to status page.
            if (httpRequest.resource === '/status') {
                var stat = status(),
                    body = '<html><h1>Status</h1><ul>';

                for (var key in stat) {
                    if (stat.hasOwnProperty(key)) {
                        body += '<li><b>' + key + '</b>: ' + stat[key] + '</li>';
                    }
                }

                body += '</ul></html>';

                var res = new HttpResponse(
                    1.1,
                    200,
                    [
                        'Content-Type: text/html',
                        'Content-Length: ' + body.length
                    ],
                    body
                ).toString();

                socket.write(res);
            }
        });
    });


    return {
        start: function (port) {
            netServer.listen(port, function (c) {

            });

            serverStarted = true;
            startDate = new Date().toLocaleDateString();
            serverPort = port;
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
