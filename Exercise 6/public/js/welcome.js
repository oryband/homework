$(document).ready(function () {
    'use strict'; 

    var $loginContainer = $('#login');
    var $loginInsteadLink = $('#register > form > a');
    var $loginUsername = $('#login_username');

    var $registerContainer = $('#register');
    var $registerInsteadLink = $('#login > form > a');

    var $error = $('#error');

    $loginInsteadLink.on('click', function () {
        $error.hide();
        $error.html('');
        $loginContainer.show();
        $registerContainer.hide();
        $loginUsername.focus();
    });

    $registerInsteadLink.on('click', function () {
        resetError();
        $registerContainer.show();
        $loginContainer.hide();
    });

    var $registerPassword = $('#password');
    var $registerPasswordConfirm = $('#password_confirm');
    var validatePassword = function () {
        if ($registerPassword.val() === $registerPasswordConfirm.val()) {
            $registerPasswordConfirm.setCustomValidity('');
        } else {
            $registerPasswordConfirm.setCustomValidity('Passwords doesn\'t match');
        }
    };

    $registerPassword.on('change', validatePassword);
    $registerPasswordConfirm.on('change', validatePassword);

    $loginContainer.find('form').on('submit', function (e) {
        var $form = $(e.target);
        $form.submitForm(function (result) {
            // success submitting form!
            if (result.success) {
                location.href = '/mail.html';
            }
        });
        return false;
    });

    $registerContainer.find('form').on('submit', function () {
        var $form = $(e.target);
        $form.submitForm(function (result) {
            // success submitting form!
            if (result.success) {
                location.href = '/mail.html';
            }
        });
        return false;
    });
});
