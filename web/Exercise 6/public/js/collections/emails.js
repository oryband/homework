'use strict';

var app = app || {};


// Email Collection.
(function () {
    // The collection of emails is backed by *localStorage* instead of a remote server.
    var EmailList = Backbone.Collection.extend({
        // Reference to this collection's model.
        model: app.Email,

        url: '/mails',

        // Emails are sorted by their original insertion order.
        // We are also sorting by priorities - the higher the better.
        comparator: function (email) {
            var date = new Date(email.get('date'));
            return -1 * date.getTime();
        }
    });


    // Create our global collection of **Emails**.
    app.Emails = new EmailList();

    // in case we are offline, fire a callback that will load emails from
    // the localStorage, which we update every time we successfully fetch.
    var offlineTimeout = setTimeout(function () {
        var emails = JSON.parse(window.localStorage.cachedEmails);

        app.Emails.reset();
        app.Emails.add(emails);
    }, 1000);

    // try to fetch emails from server, and save emails to localstorage when
    // succeeds. also clears the offline load from localstorage method timeout.
    app.Emails.fetch({success: function(collection, results, options) {
        // clear offline timeout so we won't fire it if we are online
        clearTimeout(offlineTimeout);

        // save retrieved emails to local storage
        window.localStorage.cachedEmails = JSON.stringify(results);

        // reset the emails collection in case the offline timeout fired 
        // while we are actually offline.
        app.Emails.reset();
    }});
})();
