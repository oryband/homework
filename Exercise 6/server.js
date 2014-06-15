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
        if (!user) {
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


server.post('/mails', function(request, response) {
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    var results = JSON.parse(request.body);
    for (var i = 0; i < results.length; ++i) {
      var obj = results[i];
      var id = obj._id;
      delete obj._id;
      
      schemas.Mail.update({_id: id}, obj, {upsert: true}, function (err) {
        if (err) {
          console.error('Failed saving mail on post/mails:' + err);
          return;
        }
      });
    }
    response.end();
});

server.get('/mails', function(request, response) {
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    // TODO: read user id from redis
    schemas.getMailsToUser('539d90e8e76cbd7d4bd748ac' /*params.userId*/, function (mails) {
        response.end(JSON.stringify(mails));
    });
});

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
