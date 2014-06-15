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
            'click .toggle': 'toggleCompleted',
            'dblclick #title': 'editTitle',
            'dblclick #owner': 'editOwner',
            'click .destroy': 'clear',
            'click .show': 'info',
            'click .priority_up': 'increasePriority',
            'click .priority_down': 'decreasePriority',
            'keypress .edit': 'updateOnEnter',
            'blur .edit': 'close'
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
            this.$el.toggleClass('read', this.model.get('read'));
            return this;
        },

        // Toggle the `read` state of the model.
        read: function () {
            this.model.read();
        },

        // Remove the item, destroy the model from *localStorage* and delete its view.
        clear: function () {
            this.model.destroy();
        }
    });
})(jQuery);
