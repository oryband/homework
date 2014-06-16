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

        fetchFromServer: function () {
          Backbone.emulateHTTP = true;
          Backbone.ajaxSync('read', app.Emails, {success: function (results) {
            this.each(function(model) {
              model.destroy();
            });
            
            for (var i = 0; i < results.length; ++i) {
              this.create(results[i]);
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
            return email.get('date');
        }
    });

    // Create our global collection of **Emails**.
    app.Emails = new EmailList();
})();
