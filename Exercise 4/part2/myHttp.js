var net = require('net');

var Server = function (rootFolder) {
    'use strict';

    var serverStarted = false,
        startDate = null,
        serverPort = null,
        serverCurrentRequests = 0,  // TODO: update on connection open & close.
        serverTotalRequests = 0,
        serverSuccessfulRequests = 0;


    // Immutable HTTP Request object.
     function HttpRequest(reqMethod, reqUri, reqResource, reqResPath, reqHeaders, reqBody) {
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
    }


    // Parses a HTTP request string, and returns an HttpRequest object on success,
    // returns {} otherwise.
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
        var resource = uri.substring(uri.lastIndexOf('/') + 1, uri.length - 1),
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
        var httpRequest = new HttpRequest(method, uri, resource, resPath, headers, body);

        return httpRequest;
    }


    var netServer = net.createServer(function (c) {

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

        status: function () {
            return {
                isStarted: serverStarted,
                startedDate: startDate,
                port: serverPort,
                numOfCurrentRequests: serverCurrentRequests,
                percentageOfSuccesfulRequests: serverTotalRequests === 0 ? 100 : Math.floor(serverSuccessfulRequests * 100 / serverTotalRequests)
            };
        }
    };
};


exports.createStaticHttpServer = function (rootFolder) {
    'use strict';

    return new Server(rootFolder);
};
