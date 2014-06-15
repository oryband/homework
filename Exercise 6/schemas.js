'use strict';

var mg = require('mongoose'),
    db = mg.connect('mongodb://localhost/bitsplease');

// Shorthand variables.
var oId = mg.Schema.Types.ObjectId;


exports.User = mg.model('User', new mg.Schema({
    username: String,  // User's email address is 'username@bitsplease.com'
    password: String,
    firstName: { type: String, default: '' },
    lastName: { type: String, default: '' },
    age: { type: Number, default: null }
}));


exports.Mail = mg.model('Mail', new mg.Schema({
    from: { type: oId, ref: 'User' },
    to: { type: oId, ref: 'User' },
    date: { type: Date, default: Date.now },
    subject: String,
    body: String
}));


// Currying function which executes callback on query result.
function execCallback(callback) {
    return function (err, result) {
        if (err) {
            console.error(err);
            return;
        }

        if (callback) {
            callback(result);
        }
    }
}


exports.getUserById = function (id, callback) {
    exports.User.findById(id).exec(execCallback(callback));
};


exports.getUserByUsername = function (username, callback) {
    exports.User.findOne({username: username}).exec(execCallback(callback));
};


exports.getMailById = function (id, callback) {
    exports.Mail.findById(id).exec(execCallback(callback));
};


exports.getMailsToUser = function (user, callback) {
    exports.Mail.find({to: user}).exec(execCallback(callback));
};
