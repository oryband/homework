$.fn.serializeObject = function() {
    'use strict';
    var o = {};
    var a = this.serializeArray();
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

$.fn.submitForm = function(callback) {
    'use strict';
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
