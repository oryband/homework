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
            'keypress #recipient': 'switchToSubject',
            'keypress #subject': 'switchToBody',
            'keypress #body': 'sendOnEnter',
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
            this.$recipient = this.$('#recipient');
            this.$subject = this.$('#subject');
            this.$body = this.$('#body');

            this.listenTo(app.Emails, 'add', this.addOne);
            this.listenTo(app.Emails, 'reset', this.addAll);
            this.listenTo(app.Emails, 'change:completed', this.filterOne);
            this.listenTo(app.Emails, 'all', this.render);

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

        // Generate the attributes for a new Email item.
        newAttributes: function () {
            return {
                subject: this.$subject.val().trim(),
                body: this.$body.val().trim(),
                read: false
            };
        },

        // If you hit return in the recipient field, switch focus to subject input.
        switchToSubject: function (e) {
            if (e.which !== ENTER_KEY || ! this.$recipient.val().trim()) {
                return;
            }

            this.$subject.focus();
        },

        // If you hit return in the subject field, switch focus to body input.
        switchToBody: function (e) {
            if (e.which !== ENTER_KEY || ! this.$subject.val().trim()) {
                return;
            }

            this.$body.focus();
        },

        // If you hit return in the body field, create new **Email** model,
        // persisting it to *localStorage*.
        sendOnEnter: function (e) {
          console.log(e);
            if (e.which !== ENTER_KEY ||
                ! this.$subject.val().trim() || 
                ! this.$recipient.val().trim() || 
                ! this.$body.val().trim()) {
                return;
            }

            this.createEmail();
        },

        createEmail: function () {
            app.Emails.create(this.newAttributes());
            this.$recipient.val('');
            this.$subject.val('');
            this.$body.val('');
        },

        startComposing: function () {
          this.$composeDialog.show();
          this.$composeButton.hide();
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
