'use strict';

var app = app || {};

// Overall app view.
(function ($) {
    app.AppView = Backbone.View.extend({

        // Instead of generating a new element,
        // bind to the existing skeleton of the App already present in the HTML.
        el: '#emailapp',


        // Delegated events for creating new items, and clearing completed ones.
        events: {
            'click button.compose': 'startComposing',
            'click #create_email': 'createEmail',
            'click #toggle-all': 'toggleAllRead'
        },


        // Bind relevant events to `Emails` collection, when items are added/changed.
        // Kick things off by loading preexisting emails from local storage.
        initialize: function () {
            this.allCheckbox = this.$('#toggle-all')[0];  // Select/deselect all emails checkbox.

            this.$composeDialog = this.$('#compose');
            this.$composeButton = this.$('button.compose');

            // Bind buttons to events.
            this.listenTo(app.Emails, 'add', this.addOne);
            this.listenTo(app.Emails, 'reset', this.addAll);
            this.listenTo(app.Emails, 'all', this.render);

            this.$composeDialog.on('open', this.showComposeDialog.bind(this));
            this.$composeDialog.on('close', this.stopComposing.bind(this));

            var socket = io.connect(SOCKETIO_ADDRESS);

            socket.on('error', function() { console.error(arguments); });

            // Fetch mail id and add collection when receiving new mail.
            socket.on('mail', function(mailId) {
                $.get('/mail/' + mailId, function (result) {
                    if (result.error) {
                        console.error('Failed fetching new mail id \'' + mailId + '\': ' + result.error);
                        return;
                    }

                    // Add new mail to collection.
                    var mail = new app.Email(result.mail);
                    app.Emails.create(mail);
                }.bind(this), 'json');
            }.bind(this));

            // Fetch emails from server into the local storage
            app.Emails.fetchFromServer(function () {});

            // Fetch emails from local storage
            app.Emails.fetch();
        },


        // Add a single email item to the list by creating a view for it,
        // and appending its element to the `<ul>`.
        addOne: function (email) {
            var view = new app.EmailView({ model: email });
            $('#email-list').prepend(view.render().el);
        },


        // Add all items in the **Emails** collection at once.
        addAll: function () {
            this.$('#email-list tbody').remove();
            app.Emails.fetch();
        },


        startComposing: function () {
            var view = new app.EmailComposeView({ model: null });
            this.$composeDialog.html(view.render().el);
            this.showComposeDialog();
        },


        showComposeDialog: function () {
            this.$composeDialog.show();
            this.$composeButton.hide();
        },


        stopComposing: function () {
            this.$composeButton.show();
            this.$composeDialog.hide(500);
        },


        toggleAllRead: function () {
            app.Emails.each(function (email) {
                email.save({
                    'read': true
                });
            });
        }
    });
})(jQuery);
