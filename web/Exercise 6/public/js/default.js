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


// Send mail to specified url, calling callback on success.
$.fn.sendPost = function (url, data, callback, errCallback) {
    var $error = $('#error');
    $error.hide();
    $error.html('');

    $.ajax(url, {
        type: 'POST',
        dataType: 'json',
        data: data,
        // Cache mail in localStorage for re-sending on any error.
        // We will retry on next /mail.html page load.
        error: function () {
            // Don't cache if this isn't a mail,
            // '/login' for example shouldn't be cached.
            if (url !== '/sendmail') {
                return;
            }

            // Init localStorage unsentMails data structure if not already init-ed.
            if (!localStorage.getItem('unsentMails')) {
                localStorage.setItem('unsentMails', JSON.stringify({}));
            }

            // Push current mail to list.
            var unsentMails = JSON.parse(localStorage.getItem('unsentMails'));
            unsentMails[url] = data;

            // Update unsendMails in localStorage.
            localStorage.setItem('unsentMails', JSON.stringify(unsentMails));

            // Execute error callback if given as argument.
            if (errCallback) {
                errCallback();
            }
        },
        success: function (result) {
            if (result.error) {
                $error.html(result.error);
                $error.show();
                return;
            }

            if (callback) {
                callback(result);
            }
        },
    });
};


// Submit this object the ajax way.
$.fn.submitForm = function(callback, errCallback) {
    var data = this.serializeObject(),
        url = this.attr('action');

    this.sendPost(url, data, callback, errCallback);
};
