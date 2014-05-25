var net = require('net');

var Server = function (rootFolder) {
    'use strict';

    var serverStarted = false,
        startDate = null,
        serverPort = null,
        serverCurrentRequests = 0,  // TODO: update on connection open & close.
        serverTotalRequests = 0,
        serverSuccessfulRequests = 0;

    // TODO
    var netServer = net.createServer(function(c) {
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


};
