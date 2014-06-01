/*global Backbone */
var app = app || {};

(function () {
    'use strict';

    // Todo Model
    // ----------

    // Our basic **Todo** model has `title`, `order`, and `completed` attributes.
    app.Todo = Backbone.Model.extend({
        // Default attributes for the todo
        // and ensure that each todo created has `title`, `owner`, and `completed` keys.
        // 'date' and 'priority' keys.
        defaults: {
            title: '',
            owner: '',
            completed: false,
            priority: 1, // the higher the better. new items get medium priority.
            date: (new Date()).toLocaleString()
        },

        // Toggle the `completed` state of this todo item.
        toggle: function () {
            this.save({
                completed: !this.get('completed')
            });
        }
    });
})();
