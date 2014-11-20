'use strict';

var app = app || {};

(function () {
    app.Email = Backbone.Model.extend({
        urlRoot: '/mails',

        // Default email attributes.
        defaults: {
            to: '',
            from: '',
            subject: '',
            body: '',
            read: false,
            date: (new Date()).toLocaleString()
        },


        // Set the `read` state of this email.
        read: function () {
            this.save({ read: true });
        }
    });
})();
