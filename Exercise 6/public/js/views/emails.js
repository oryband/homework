'use strict';

var app = app || {};


// Email Item View
(function ($) {
    // The DOM element for an email item.
    app.EmailView = Backbone.View.extend({
        // is a list tag.
        tagName:  'tbody',

        // Cache template function for a single item.
        template: _.template($('#email-template').html()),

        // DOM events specific to an item.
        events: {
            'click .delete': 'clear',
            'click .reply': 'composeReply',
            'click tr.email': 'toggleContent',
            'click tr.mail_content .close': 'toggleContent'
        },


        // The EmailView listens for changes to its model, re-rendering.
        // Since there's a one-to-one correspondence between a **Email** and a **EmailView** in this app,
        // we set a direct reference on the model for convenience.
        initialize: function () {
            this.$composeDialog = $('#compose');

            this.listenTo(this.model, 'change', this.render);
            this.listenTo(this.model, 'destroy', this.remove);
        },


        // Re-render titles of the email item.
        render: function () {
            this.$el.html(this.template(this.model.toJSON()));
            this.$content = this.$('tr.mail_content');
            return this;
        },


        toggleContent: function () {
            this.$el.toggleClass('content_shown');

            if (!this.model.get('read')) {
                this.model.read();
            }
        },


        composeReply: function () {
            var view = new app.EmailComposeView({ model: this.model });
            this.$composeDialog.html(view.render().el);
            this.$composeDialog.trigger('open');

            // Stop bubbling up click event to the tr.email element.
            return false;
        },


        // Remove the item, destroy the model from *localStorage* and delete its view.
        clear: function () {
            this.model.id = this.model.get('_id');
            this.model.destroy();
        }
    });
})(jQuery);
