'use strict';

var app = app || {};


// Email Compose View
(function ($) {
    // The DOM element for an email item.
    app.EmailComposeView = Backbone.View.extend({
        tagName: 'div',

        template: _.template($('#compose_dialog-template').html()),

        // The DOM events specific to an item.
        events: {
            'keypress #recipient': 'switchToSubject',
            'keypress #subject': 'switchToBody',
            'click .close_dialog': 'clear',
            'click #create_email': 'createEmail'
        },


        // The EmailView listens for changes to its model, re-rendering.
        // Since there's a one-to-one correspondence between a **Email** and a **EmailView** in this app,
        // we set a direct reference on the model for convenience.
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


        createEmail: function () {
            // submit the form
            this.$form.submitForm(function () {
                // close the dialog
                this.$el.parent().trigger('close');

                // save recipient to show in success message
                var recipient = this.$recipient.val();

                // reset the form
                this.$recipient.val('');
                this.$subject.val('');
                this.$body.val('');

                var $success = $('#success');
                $success.html('Mail was sent successfully to ' + recipient + '!');
                $success.show();
                setTimeout(function () {
                    // hide success message
                    $success.hide();
                }.bind(this), 2000);
            }.bind(this));
        },


        // Switch focus to subject input if you hit return in the recipient field.
        switchToSubject: function (e) {
            if (e.which !== ENTER_KEY || ! this.$recipient.val().trim()) {
                return;
            }

            this.$subject.focus();
        },


        // Switch focus to body input if you hit return in the subject field.
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

            this.$form = this.$('form');
            this.$recipient = this.$('#recipient');
            this.$subject = this.$('#subject');
            this.$body = this.$('#body');

            // If the model exists, it means it's a reply dialog.
            // We should focus on the body textarea, as the rest is pre-filled.
            if (this.model) {
                setTimeout(this.$body.focus.bind(this.$body), 1);
            } else {
                // Otherwise, we should focus on the recipient field.
                setTimeout(this.$recipient.focus.bind(this.$recipient), 1);
            }
            return this;
        }
    });
})(jQuery);
