/*global Backbone, jQuery, _, ENTER_KEY */
var app = app || {};

(function ($) {
    'use strict';

    // Email Item View
    // --------------

    // The DOM element for a email item...
    app.EmailView = Backbone.View.extend({
        //... is a list tag.
        tagName:  'tbody',

        // Cache the template function for a single item.
        template: _.template($('#email-template').html()),

        // The DOM events specific to an item.
        events: {
            'click .delete': 'clear',
            'click tr.email': 'toggleContent',
            'click tr.mail_content .close': 'toggleContent'
        },

        // The EmailView listens for changes to its model, re-rendering. Since there's
        // a one-to-one correspondence between a **Email** and a **EmailView** in this
        // app, we set a direct reference on the model for convenience.
        initialize: function () {
          this.listenTo(this.model, 'change', this.render);
          this.listenTo(this.model, 'destroy', this.remove);
        },

        // Re-render the titles of the email item.
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

        // Remove the item, destroy the model from *localStorage* and delete its view.
        clear: function () {
            this.model.destroy();
        }
    });
})(jQuery);
