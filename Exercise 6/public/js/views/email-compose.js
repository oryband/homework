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
