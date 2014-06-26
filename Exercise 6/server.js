'use strict';

// User created modules.
var http = require('./http'),
    settings = require('./settings'),
    schemas = require('./schemas');

// 3rd-party modules.
var io = require('socket.io')(settings.SOCKETIO_PORT),
    redis = require('redis'),
    _ = require('underscore')._;

// Module objects' instances.
var rc = redis.createClient(),
    server = http.createHTTPServer('./public'),
    sockets = {};


// Redis callbacks:

// Create UUID.
var guid = (function() {
    function s4() {
        return Math.floor((1 + Math.random()) * 0x10000).toString(16).substring(1);
    }

    return function() {
        return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
            s4() + '-' + s4() + s4() + s4();
    };
})();


rc.on('error', function (err) {
    console.error('Redis error: ' + err);
});


// Socket.IO callbacks:

// Retrieve UUID from cookie.
function getUUIDFromCookie(cookie) {
    var parts = cookie.split('; '),
        keyval;

    // Search cookie keys/values for UUID.
    for (var i=0; i < parts.length; i++) {
        keyval = parts[i].split('=');
        if (keyval[0] === 'uuid') {  // Return UUID when found.
            return keyval[1];
        }
    }

    // Return null if UUID wasn't in cookie.
    return null;
}


io.on('connection', function(socket) {
    var uuid = getUUIDFromCookie(socket.request.headers.cookie) || '';

    // Remember socket conection.
    rc.get(uuid, function (err, userId) {
        if (err) {
            console.error('Error GETting Redis UUID: ' + err);
            return;
        }

        // Delete socket on disconnect.
        socket.on('disconnect', function() {
            delete sockets[userId];
        });

        sockets[userId] = socket;

        socket.emit('welcome');
    });
});


// URI callbacks:

// Send a 'Welcome to Bitsplease!' mail to a new user.
function sendWelcomeMail(user) {
    // Get/create the 'welcome' user, which will send a 'Welcome!' mail to new user.
    schemas.User.findOneAndUpdate(
        { username: 'welcome' },
        { username: 'welcome', password: 'welcome', firstName: 'Welcome', lastName: 'Bitsplease' },
        { upsert: true },
        function (err, welcome) {
            if (err) {
                console.error('Error get/create \'welcome\' user: ' + err);
                return;
            }

            // Send 'Welcome!' mail to new user.
            new schemas.Mail({
                from: welcome,
                to: user,
                subject: 'Welcome to Bitsplease Mail!',
                body: 'Dear ' + user.firstName + ',\n' +
                    'Welcome to Bitsplease Mail!\n\n' +
                    'Hope you will enjoy using our service as much as we did building it.\n\n' +
                    'Yours,\n' +
                    'The Bitsplease Team.'
            }).save(function (err) {
                if (err) {
                    console.error('Error sending welcome mail to user \'' + user.username + '\': ' + err);
                }
            });
        }
    );
}


// Generate a new UUID, save in redis (with expiration time),
// and set UUID to be replied as a cookie.
function setUUIDasCookie(response, user) {
    var uuid = guid(),
        now = new Date();

    rc.setex(uuid, settings.REQUESTS_TIME_THRESHOLD_IN_SEC, user._id);

    // Set cookie expiration time.
    now.setTime(now.getTime() + 1000 * settings.REQUESTS_TIME_THRESHOLD_IN_SEC);
    response.headers['Set-Cookie'] = 'uuid=' + uuid + '; ' + now.toUTCString();
}


// Register user.
server.post('/register', function(request, response) {
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    // Fetch POST body parameters.
    var params = http.parsePostBody(request);

    // Return error if username/password are missing, or if username is already taken.
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
        schemas.User({
            username: params.username,
            password: params.password,
            firstName: params.firstName || '',
            lastName: params.lastName || '',
            age: params.age || null
        }).save(function (err, newUser) {
            if (err) {
                console.error('Error saving new user \'' + params.username + '\': ' + err);
                return;
            }

            // Send welcome mail to newly created user.
            sendWelcomeMail(newUser);

            // Generate a new UUID and set as response cookie.
            setUUIDasCookie(response, newUser);

            // 'Redirect' to mail.html page.
            response.end(JSON.stringify({
                'success': true,
                'location': 'mail.html'
            }));
        });
    });
});


// Login user.
server.post('/login', function(request, response) {
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    // Parse POST body parameters.
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

        // Generate a new UUID and set as repsonse cookie.
        setUUIDasCookie(response, user);

        // If username and password match, redirect to 'mail.html' page.
        response.end(JSON.stringify({
            'success': true,
            'location': 'mail.html'
        }));
    });
});


// Delete mail by mail-ID, given as parameter.
server.post('/mails/:id', function(request, response) {
    var mailId = request.params.id;

    // Fetch the current user (the recipient of the mail).
    // This will make sure no one is deleting someone else's mail.

    // Fetch UUID from cookie.
    var uuid = getUUIDFromCookie(request.headers['Cookie']) || '';

    // Get user ID and fetch user object.
    rc.get(uuid, function (err, userId) {
        if (err) {
            // Error probably means user didn't log in,
            // so now UUID was generated for him.
            console.error('Error GETting Redis UUID: ' + err);
            response.end(JSON.stringify( { 'error': 'Must be logged in to delete mail.' } ));
            return;
        }

        schemas.getUserById(userId, function (user) {
            // Return error if user isn't logged in.
            if (!user) {
                response.end(JSON.stringify( { 'error': 'Must be logged in to delete an email.' } ));
                return;
            }

            schemas.deleteSpecificMailOfUser(user, mailId, function () {
                response.end(JSON.stringify({ success: true }));
            });
        });
    });
});


// Update mail data, usually for setting 'read' field to false.
server.post('/mails', function(request, response) {
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    // Fetch mails and make sure it's an array.
    var mails = JSON.parse(request.body);
    mails = _.isArray(mails) ? mails : [mails];

    // Update each mail.
    var mail, obj;
    for (var i=0; i < mails.length; i++) {
        mail = mails[i];

        // We're only going to ever update the 'read' field of mails.
        // The reset of the fields should never change.
        obj = { read: mail.read };

        schemas.Mail.update({ _id: mail._id }, obj, function (err) {
            if (err) {
                console.error('Failed saving mail on POST /mails: ' + err);
            }
        });
    }

    response.end();
});


// Fetch user's mail list.
server.get('/mails', function(request, response) {
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    // Fetch UUID from cookie.
    var uuid = getUUIDFromCookie(request.headers['Cookie']) || '';

    // Get user ID and fetch all mails whose this user is the recipient (is the 'to' field).
    rc.get(uuid, function (err, userId) {
        if (err) {
            console.error('Error GETting Redis UUID: ' + err);
            return;
        }

        // Get mails sent to user, and return them as json object.
        schemas.getMailsToUser(userId, function (mails) {
            response.end(JSON.stringify(mails));
        });
    });
});


// Send mail from user.
server.post('/sendmail', function (request, response) {
    response.status = 200;
    response.headers['Content-Type'] = 'application/json';

    // Fetch POST body parameters.
    var params = http.parsePostBody(request);

    // Check for missing parameters.
    if (!params.to) {
        response.end(JSON.stringify( { 'error': 'Missing \'to\' field.' } ));
        return;
    } else if (!params.subject) {
        response.end(JSON.stringify( { 'error': 'Missing \'subject\' field.' } ));
        return;
    } else if (!params.body) {
        response.end(JSON.stringify( { 'error': 'Missing \'body\' field.' } ));
        return;
    }

    var to = params.to,
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

        // Fetch UUID from cookie.
        var uuid = getUUIDFromCookie(request.headers['Cookie']) || '';

        // Get user ID and fetch user object.
        rc.get(uuid, function (err, userId) {
            if (err) {
                console.error('Error GETting Redis UUID: ' + err);
                return;
            }

            schemas.getUserById(userId, function (user) {
                // Return error if user isn't logged in.
                if (!user) {
                    response.end(JSON.stringify( { 'error': 'Must be logged in to send mail.' } ));
                    return;
                }

                fromUser = user;

                // Send mail.
                new schemas.Mail({ from: fromUser, to: toUser, subject: subject, body: body })
                .save(function (err, mail) {
                    if (err) {
                        console.error('Error sending mail from user \'' + fromUser.username + '\' to user \'' + toUser.username + '\': ' + err);
                        return;
                    }

                    // After mail has been sent, we reply with success to the client.
                    // Socket.IO notfications shouldn't slow down the client who sent the mail.
                    response.end(JSON.stringify({}));

                    // Notify recipient user of new mail.
                    if (!sockets[toUser._id]) {
                        console.error('Missing UUID from sockets: \'' + uuid + '\'.');
                        return;
                    }

                    sockets[userId].emit('mail', mail._id);
                });
            });
        });
    });
});


// Get a specific mail by mail-ID.
server.get('/mail/:id', function (request, response) {
    var mailId = request.params.id;

    // Fetch the current user (the recipient of the mail). This will make sure
    // no one is requesting someone else's mail.

    // Fetch UUID from cookie.
    var uuid = getUUIDFromCookie(request.headers['Cookie']) || '';

    // Get user ID and fetch user object
    rc.get(uuid, function (err, userId) {
        if (err) {
            console.error('Error GETting Redis UUID: ' + err);

            // Error fetching Redis UUID key probably means user isn't logged in.
            response.end(JSON.stringify( { 'error': 'Must be logged in to receive mail.' } ));
            return;
        }

        schemas.getUserById(userId, function (user) {
            // Return error if user isn't logged in.
            if (!user) {
                response.end(JSON.stringify( { 'error': 'Must be logged in to receive mail.' } ));
                return;
            }

            schemas.getSpecificMailToUser(user, mailId, function (mail) {
                response.end(JSON.stringify({success: true, mail: mail}));
            });
        });
    });
});


server.start(settings.TEST_PORT);
