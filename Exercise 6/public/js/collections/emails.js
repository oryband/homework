/*global Backbone */
var app = app || {};

(function () {
    'use strict';

    // Email Collection
    // ---------------

    // The collection of emails is backed by *localStorage* instead of a remote
    // server.
    var EmailList = Backbone.Collection.extend({
        // Reference to this collection's model.
        model: app.Email,

        url: '/mails',

        // Save all of the email items under the `"emails"` namespace.
        localStorage: new Backbone.LocalStorage('emails-backbone'),

        fetchFromServer: function (callback) {
            Backbone.emulateHTTP = true;

            // fetch emails from server
            Backbone.ajaxSync('read', app.Emails, {success: function (results) {
                // when we receive a response, clean the local storage
                var model;
                while ((model = this.shift())) {
                    this.localStorage.destroy(model);
                }

                // cache emails to local storage
                for (var i = 0; i < results.length; ++i) {
                    this.localStorage.create(new app.Email(results[i]));
                }

                if (callback) {
                    callback();
                }
            }.bind(this)});
        },

        syncWithServer: function (options) {
            Backbone.emulateHTTP = true;
            Backbone.ajaxSync('update', app.Emails, {success: console.log});
        },

        // Emails are sorted by their original insertion order.
        // we are also sorting by priorities - the higher the better.
        comparator: function (email) {
            var date = new Date(email.get('date'));
            return -1 * date.getTime();
        }
    });

    // Create our global collection of **Emails**.
    app.Emails = new EmailList();
})();
