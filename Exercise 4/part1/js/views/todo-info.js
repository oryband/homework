/*global Backbone, jQuery, _ */
var app = app || {};

(function ($) {
    'use strict';

    app.TodoInfoView = Backbone.View.extend({
        tagName: 'div',

        template: _.template($('#item-info-template').html()),

        intialize: function () {
            this.listenTo(app.Todos, 'info', this.render);
        },

        render: function () {
            this.$el.html(this.template(this.model.toJSON()));
            return this;
        }

    });
})(jQuery);
