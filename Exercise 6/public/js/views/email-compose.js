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
            this.listenTo(this.model, 'destroy', this.remove);
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
            return this;
        }
    });
})(jQuery);
