/*global Backbone, jQuery, _, ENTER_KEY */
var app = app || {};

(function ($) {
    'use strict';

    // Email Compose View
    // --------------

    // The DOM element for a email item...
    app.EmailComposeView = Backbone.View.extend({
        tagName:  'div',

        template: _.template($('#compose_dialog-template').html()),

        // The DOM events specific to an item.
        events: {
            'keypress #recipient': 'switchToSubject',
            'keypress #subject': 'switchToBody',
            'keypress #body': 'sendOnEnter',
            'click .close_dialog': 'clear',
            'click #createEmail': 'createEmail'
        },

        // The EmailView listens for changes to its model, re-rendering. Since there's
        // a one-to-one correspondence between a **Email** and a **EmailView** in this
        // app, we set a direct reference on the model for convenience.
        initialize: function () {
          if (this.model) {
            this.listenTo(this.model, 'change', this.render);
            this.listenTo(this.model, 'destroy', this.clear);
          }
        },

        getJSON: function () {
          if (this.model !== null) {
            return this.model.toJSON();
          }

          return {
            from: '',
            recipient: '',
            subject: '',
            body: ''
          };
        },

        // If you hit return in the body field, create new **Email** model,
        // persisting it to *localStorage*.
        sendOnEnter: function (e) {
            if (e.which !== ENTER_KEY ||
                ! this.$subject.val().trim() || 
                ! this.$recipient.val().trim() || 
                ! this.$body.val().trim()) {
                return;
            }

            this.createEmail();
            this.$el.parent().trigger('close');
            return false;
        },

        // Generate the attributes for a new Email item.
        newAttributes: function () {
            return {
                from: '********',
                to: this.$recipient.val().trim(),
                subject: this.$subject.val().trim(),
                body: this.$body.val().trim(),
                read: false
            };
        },

        createEmail: function () {
            app.Emails.create(this.newAttributes());
            this.$recipient.val('');
            this.$subject.val('');
            this.$body.val('');
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
        
        clear: function () {
          this.$el.parent().trigger('close');
          this.$el.remove();
        },

        render: function () {
            this.$el.html(this.template(this.getJSON()));

            this.$recipient = this.$('#recipient');
            this.$subject = this.$('#subject');
            this.$body = this.$('#body');

            // if the model exists, it means it's a reply dialog.
            // we should focus on the body textarea, as the rest is pre-filled.
            if (this.model) {
              setTimeout(this.$body.focus.bind(this.$body), 1);
            } else {
              // otherwise, we should focus on the recipient field
              setTimeout(this.$recipient.focus.bind(this.$recipient), 1);
            }
            return this;
        }
    });
})(jQuery);
