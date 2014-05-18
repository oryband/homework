/*global Backbone, jQuery, _, ENTER_KEY */
var app = app || {};

(function ($) {
    'use strict';

    // Todo Item View
    // --------------

    // The DOM element for a todo item...
    app.TodoView = Backbone.View.extend({
        //... is a list tag.
        tagName:  'li',

        // Cache the template function for a single item.
        template: _.template($('#item-template').html()),

        // The DOM events specific to an item.
        events: {
            'click .toggle': 'toggleCompleted',
            'dblclick #title': 'editTitle',
            'dblclick #owner': 'editOwner',
            'click .destroy': 'clear',
            'click .show': 'info',
            'keypress .edit': 'updateOnEnter',
            'blur .edit': 'close'
        },

        // The TodoView listens for changes to its model, re-rendering. Since there's
        // a one-to-one correspondence between a **Todo** and a **TodoView** in this
        // app, we set a direct reference on the model for convenience.
        initialize: function () {
            this.listenTo(this.model, 'change', this.render);
            this.listenTo(this.model, 'destroy', this.remove);
            this.listenTo(this.model, 'visible', this.toggleVisible);
        },

        // Re-render the titles of the todo item.
        render: function () {
            this.$el.html(this.template(this.model.toJSON()));
            this.$el.toggleClass('completed', this.model.get('completed'));
            this.toggleVisible();
            this.$title = this.$('#title-input');
            this.$owner = this.$('#owner-input');
            return this;
        },

        toggleVisible: function () {
            this.$el.toggleClass('hidden', this.isHidden());
        },

        isHidden: function () {
            var isCompleted = this.model.get('completed');
            return (// hidden cases only
                    (!isCompleted && app.TodoFilter === 'completed') ||
                    (isCompleted && app.TodoFilter === 'active')
                   );
        },

        // Toggle the `"completed"` state of the model.
        toggleCompleted: function () {
            this.model.toggle();
        },

        // Switch this view into `"editing"` mode, displaying the title field.
        editTitle: function () {
            this.$el.addClass('editing');
            this.$title.focus();
        },

        // Same, but for owner field.
        editOwner: function () {
            this.$el.addClass('editing');
            this.$owner.focus();
        },

        // Close the `"editing"` mode, saving changes to the todo.
        close: function () {
            var titleVal = this.$title.val().trim(),
                ownerVal = this.$owner.val().trim();

            if (titleVal || ownerVal) {
                this.model.save({ title: titleVal, owner: ownerVal });
            } else {
                this.clear();
            }

            this.$el.removeClass('editing');
        },

        // If you hit `enter`, we're through editing the item.
        updateOnEnter: function (e) {
            if (e.which === ENTER_KEY) {
                this.close();
            }
        },

        // Remove the item, destroy the model from *localStorage* and delete its view.
        clear: function () {
            this.model.destroy();
        },

        // Show item meta data next to it
        info: function () {
        }
    });
})(jQuery);
