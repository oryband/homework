'use strict';

// Custom jQuery object functions:

// Serialize array to object.
$.fn.serializeObject = function() {
    var o = {},
    a = this.serializeArray();

    $.each(a, function() {
        if (o[this.name] !== undefined) {
            if (!o[this.name].push) {
                o[this.name] = [o[this.name]];
            }
            o[this.name].push(this.value || '');
        } else {
            o[this.name] = this.value || '';
        }
    });

    return o;
};


// Submit this object the ajax way.
$.fn.submitForm = function(callback) {
    var $error = $('#error');
    $error.hide();
    $error.html('');

    $.post(
        this.attr('action'),
        this.serializeObject(),
        function (result) {
            if (result.error) {
                $error.html(result.error);
                $error.show();
                return;
            }

            if (callback) {
                callback(result);
            }
        },
        'json'
    );
};
