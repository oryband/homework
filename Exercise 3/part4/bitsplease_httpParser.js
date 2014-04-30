"use strict";

// Immutable HTTP Request object.
var HttpRequest = function (reqMethod, reqUri, reqResource, reqResPath, reqHeaders, reqBody) {
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


// Parses a HTTP request string, and returns an HttpRequest object on success,
// returns {} otherwise.
function parseRequest(req) {
    if (!req) {
        console.error("request is null.");
        return {};
    }

    // Parse request line.
    var lines = req.split("\r\n"),
        reqLine = lines[0].split(" ");

    if (reqLine.length !== 3) {
        console.error("Request line not valid.");
        return {};
    }

    // Parse request method.
    var method = reqLine[0],
        uri = reqLine[1],
        version = reqLine[2];

    if (method !== "GET" && method !== "POST") {
        console.error("Method '%s' not GET or POST.", method);
        return {};
    }

    // Parse request URI.
    if (! /((ftp|http|https):\/\/(\w+:{0,1}\w*@)?(\S+)(:[0-9]+)?)?(\/|\/([\w#!:.?+=&%@!\-\/]))?/.test(uri, "i")) {
        console.error("Bad URI '%s'", uri);
        return {};
    }

    // Parse HTTP version.
    if (version !== "HTTP/1.0" && version !== "HTTP/1.1") {
        console.error("Bad version.");
        return {};
    }

    // Parse path and resource.
    var resource = uri.substring(uri.lastIndexOf("/") + 1, uri.length - 1),
        resPath = uri.substring(0, uri.lastIndexOf("/"));

    // Parse headers.
    var i,
        headers = {};
    for (i = 1; i < lines.length; ++i) {
        var line = lines[i];

        // Headers and body are separated by CRLF. This will result in an
        // empty line. If detected, break the headers parsing and start parsing
        // the body lines.
        if (line === "") {
            break;
        }

        var parts = line.split(": ");
        if (parts.length !== 2) {
            console.error("Invalid header received '%s'", line);
            return {};
        }

        var key = parts[0],
            value = parts[1];

        headers[key] = value;
    }

    // Parse body.
    i += 2;
    var body = lines.splice(i, lines.length).join("\r\n");

    // Build response.
    var httpRequest = new HttpRequest(method, uri, resource, resPath, headers, body);

    return httpRequest;
}


// For testing purposes, see HTML page.
var test = [
    "GET /Protocols/rfc2616/rfc2616-sec5.html HTTP/1.1",
    "Host: www.w3.org",
    "Connection: keep-alive",
    "Cache-Control: no-cache",
    "Pragma: no-cache",
    "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/34.0.1847.131 Safari/537.36",
    "Referer: https://www.google.com/",
    "Accept-Encoding: gzip,deflate,sdch",
    "Accept-Language: en-US,en;q=0.8,he;q=0.6",
    "\r\n",
    "<html>",
    "</html>",
].join("\r\n");
