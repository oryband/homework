'use strict';

var mg = require('mongoose'),
    db = mg.connect('mongodb://localhost/db');

// Shorthand variables.
var oId = mg.Schema.Types.ObjectId;


// Contact details.
exports.Contact = mg.model('Contact', new mg.Schema({
    address: String,
    firstName: { type: String, default: '' },
    lastName: { type: String, default: '' },
    age: { type: Number, default: null }
}));


exports.User = mg.model('User', new mg.Schema({
    name: String,
    password: String,
    contact: { type: oId, ref: 'Contact' },
}));


exports.Mail = mg.model('Mail', new mg.Schema({
    from: { type: oId, ref: 'Contact' },
    to: { type: oId, ref: 'User' },
    date: { type: Date, default: Date.now },
    subject: String,
    body: String
}));


exports.getMailsByUser = function (user, callback) {
    Mail.find({to: user}).exec(function (err, result) {
        if (err) {
            console.error(err);
            return;
        }

        if (callback) {
            callback(result);
        }
    });
};
