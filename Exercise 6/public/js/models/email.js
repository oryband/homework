/*global Backbone */
var app = app || {};

(function () {
    'use strict';

    // Email Model
    // ----------

    app.Email = Backbone.Model.extend({

        url: '/mails',

        // Default attributes for an email
        defaults: {
            to: '',
            from: '',
            subject: '',
            body: '',
            read: false,
            date: (new Date()).toLocaleString()
        },

        // Set the `read` state of this email
        read: function () {
            this.save({read: true});
        }
    });
})();
