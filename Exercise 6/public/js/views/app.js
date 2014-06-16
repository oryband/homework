/*global Backbone, jQuery, _, ENTER_KEY */
var app = app || {};

(function ($) {
    'use strict';

    // The Application
    // ---------------

    // Our overall **AppView** is the top-level piece of UI.
    app.AppView = Backbone.View.extend({

        // Instead of generating a new element, bind to the existing skeleton of
        // the App already present in the HTML.
        el: '#emailapp',

        // Delegated events for creating new items, and clearing completed ones.
        events: {
            'click button.compose': 'startComposing',
            'click #create_email': 'createEmail',
            'click #toggle-all': 'toggleAllRead'
        },

        // At initialization we bind to the relevant events on the `Emails`
        // collection, when items are added or changed. Kick things off by
        // loading any preexisting emails that might be saved in *localStorage*.
        initialize: function () {
            this.allCheckbox = this.$('#toggle-all')[0];
            this.$composeDialog = this.$('#compose');
            this.$composeButton = this.$('button.compose');

            this.listenTo(app.Emails, 'add', this.addOne);
            this.listenTo(app.Emails, 'reset', this.addAll);
            this.listenTo(app.Emails, 'change:completed', this.filterOne);
            this.listenTo(app.Emails, 'all', this.render);

            this.$composeDialog.on('open', this.showComposeDialog.bind(this));
            this.$composeDialog.on('close', this.stopComposing.bind(this));
    
            // fetch emails from the server into the local storage
            app.Emails.fetchFromServer(function () {
            });

            // fetch emails from local storage
            app.Emails.fetch();
        },

        // Re-rendering the App just means refreshing the statistics -- the rest
        // of the app doesn't change.
        render: function () {
        },

        // Add a single email item to the list by creating a view for it, and
        // appending its element to the `<ul>`.
        addOne: function (email) {
            var view = new app.EmailView({ model: email });

            $('#email-list').append(view.render().el);
        },

        // Add all items in the **Emails** collection at once.
        addAll: function () {
            this.$('#email-list tbody').remove();
            app.Emails.each(this.addOne, this);
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
