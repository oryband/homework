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
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    var params = http.parsePostBody(request);

    // Return error if username/password are missing,
    // or if username is already taken.
    if (!params.username) {
        response.end(JSON.stringify( { 'error': 'Missing user name.' } ));
        return;
    } else if (!params.password) {
        response.end(JSON.stringify( { 'error': 'Missing password.' } ));
        return;
    }

    schemas.getUserByUsername(params.username, function (user) {
        // Return error if username is already taken.
        if (user) {
            response.end(JSON.stringify( { 'error': 'User name \'' + params.username + '\' already taken.' } ));
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

        // Get or create the 'welcome' user, which will send a 'Welcome!' mail to new user.
        var welcome = schemas.User.update(
            { username: 'welcome' },
            { username: 'welcome', password: 'welcome', firstName: 'Welcome', lastName: 'Bitsplease' },
            { upsert: true },
            function (err) { if (err) { console.error('Failed saving mail on POST /mails:' + err); } }
        );

        console.log(welcome);

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

        // FIXME Why mails are missing 'from' field?!?

        // Redirect to mail.html page.
        response.end(JSON.stringify({
            'success': true,
            'location': 'mail.html'
        }));
    });
});


// Login user.
server.post('/login', function(request, response) {
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    var params = http.parsePostBody(request);

    // Check for username and password errors.
    if (!params.username) {
        response.end(JSON.stringify( { 'error': 'Missing user name.' } ));
        return;
    } else if (!params.password) {
        response.end(JSON.stringify( { 'error': 'Missing password.' } ));
        return;
    }

    schemas.getUserByUsername(params.username, function(user) {
        // Return error if username doesn't exist.
        if (!user) {
            response.end(JSON.stringify( { 'error': 'User name \'' + params.username + '\' doesn\'t exist.' } ));
            return;
        } else if (user.password !== params.password) {
            // Return error if password doesn't match.
            response.end(JSON.stringify( { 'error': 'Wrong password.' } ));
            return;
        }

        // If username and password match, redirect to 'mail.html' page.
        response.end(JSON.stringify({
            'success': true,
            'location': 'mail.html'
        }));
    });
});


// Update mail data, usually for setting 'read' field to false.
server.post('/mails', function(request, response) {
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    var mails = JSON.parse(request.body);
    for (var i=0; i < mails.length; i++) {
        var mail = mails[i],
            id = mail._id;

        // Update or create mail.
        delete mail._id;
        schemas.Mail.update({ _id: id }, mail, { upsert: true }, function (err) {
            if (err) {
                console.error('Failed saving mail on POST /mails:' + err);
            }
        });
    }

    response.end();
});


// Fetch user's mail list.
server.get('/mails', function(request, response) {
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    var userId = params.userId;  // TODO: read user id from redis
    schemas.getMailsToUser(userId, function (mails) {
        response.end(JSON.stringify(mails));
    });
});


// Send mail from user.
// TODO this seems unnecessary now that we have /mails callback.
server.post('/sendmail', function (request, response) {
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    var params = http.parsePostBody(request);

    // Check for missing parameters.
    if (!params.to) {
        response.end(JSON.stringify( { 'error': 'Missing \'to\' field.' } ));
        return;
    } else if (!params.from) {
        response.end(JSON.stringify( { 'error': 'Missing \'from\' field.' } ));
        return;
    } else if (!params.subject) {
        response.end(JSON.stringify( { 'error': 'Missing \'subject\' field.' } ));
        return;
    } else if (!params.body) {
        response.end(JSON.stringify( { 'error': 'Missing \'body\' field.' } ));
        return;
    }

    var to = params.to,
        from = params.from,
        subject = params.subject,
        body = params.body;

    var toUser, fromUser;

    // Fetch sender and recipient users.
    schemas.getUserByUsername(to, function (user) {
        // Return error if sender user doesn't exist.
        if (!user) {
            response.end(JSON.stringify( { 'error': 'User ' + to + ' doesn\'t exist.' } ));
            return;
        }

        toUser = user;

        schemas.getUserByUsername(from, function (user) {
            // Return error if recipient user doesn't exist.
            if (!user) {
                response.end(JSON.stringify( { 'error': 'User ' + to + ' doesn\'t exist.' } ));
                return;
            }

            fromUser = user;

            var mail = new schemas.Mail({
                from: fromUser,
                to: toUser,
                subject: subject,
                body: body
            });

            mail.save();

            // TODO notify user of mail.id.
        });
    });
});


server.start(settings.TEST_PORT);
