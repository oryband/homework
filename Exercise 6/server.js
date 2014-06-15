'use strict';

// 3rd-party modules.
var socket = require('socket.io'),  // TODO add listen.
    redis = require('redis');

// User created modules.
var http = require('./http'),
    settings = require('./settings'),
    schemas = require('./schemas');

// Module objects' instances.
var rc = redis.createClient(),
    server = http.createHTTPServer('./public');


// Register URI events:

// Register user.
server.post('/register', function(request, response) {
    var params = request.params;

    // Return error if username/password are missing,
    // or if username is already taken.
    if (!params.username) {
        response.status = 400;  // Bad request.
        response.headers['Content-Type'] = 'application/json';
        response.end(JSON.stringify( { 'error': 'Missing user name.' } ));
        return;
    } else if (!params.password) {
        response.status = 400;
        response.headers['Content-Type'] = 'application/json';
        response.end(JSON.stringify( { 'error': 'Missing password.' } ));
        return;
    }

    schemas.getUserByUsername(params.username, function (user) {
        // Return error if username is already taken.
        if (!user) {
            response.status = 400;
            response.headers['Content-Type'] = 'application/json';
            response.end(JSON.stringify( { 'error': 'User name already taken.' } ));
            return;
        }

        // Add new user to db.
        var newUser = schemas.User({
            username: params.username,
            password: params.password,
            firstName: params.firstName || '',
            lastName: params.lastName || '',
            age: params.age || null
        });

        newUser.save();

        // Get or create the 'Welcome' user, which will send a 'Welcome!' mail to new user.
        var welcome;
        schemas.getUserByUsername('welcome', function(user) {
            if (user) {
                welcome = user;
            } else {
                // Create 'welcome' user if not yet created.
                welcome = new schemas.User({
                    username: 'welcome',
                    password: 'welcome',
                    firstName: 'Welcome',
                    lastName: 'Bitsplease'
                });

                welcome.save();
            }
        });

        // Send 'Welcome!' mail to new user.
        new schemas.Mail({
            from: welcome,
            to: newUser,
            subject: 'Welcome to Bitsplease Mail!',
            body: 'Dear ' + newUser.firstName + ',\n' +
                'Welcome to Bitsplease Mail!\n\n' +
                'Hope you will enjoy using our service as much as we did building it.\n\n' +
                'Yours,\n' +
                'The Bitsplease Team.'
        }).save();

        // Redirect to mail.html page.
        response.status = 302;  // Found.
        response.headers['Content-Type'] = 'application/json';
        response.headers['Location'] = 'mail.html';
        response.end(JSON.stringify( { 'success': 'User created.' } ));
    });
});


// Login user.
server.post('/login', function(request, response) {
    var params = request.params;

    // Checks for username and password errors.
    if (!params.username) {
        response.status = 400;  // Bad request.
        response.headers['Content-Type'] = 'application/json';
        response.end(JSON.stringify( { 'error': 'Missing user name.' } ));
        return;
    } else if (!params.password) {
        response.status = 400;
        response.headers['Content-Type'] = 'application/json';
        response.end(JSON.stringify( { 'error': 'Missing password.' } ));
        return;
    }

    schemas.getUserByUsername(request.params.username, function(user) {
        // Return error if username doesn't exist.
        if (!user) {
            response.status = 404;  // Not found.
            response.headers['Content-Type'] = 'application/json';
            response.end(JSON.stringify( { 'error': 'User name doesn\'t exist.' } ));
            return;
        } else if (user.password !== params.password) {
            // Return error if password doesn't match.
            response.status = 403;  // Unauthorized.
            response.headers['Content-Type'] = 'application/json';
            response.end(JSON.stringify( { 'error': 'Wrong password.' } ));
            return;
        }

        // If username and password match, redirect to 'mail.html' page.
        response.status = 302;  // Found.
        response.headers['Content-Type'] = 'application/json';
        response.headers['Location'] = 'mail.html';
        response.end(JSON.stringify( { 'success': 'Login successful.' } ));
    });
});


server.start(settings.TEST_PORT);
