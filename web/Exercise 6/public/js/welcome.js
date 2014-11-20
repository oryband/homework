'use strict';

$(document).ready(function () {
    var $loginContainer = $('#login'),
        $loginInsteadLink = $('#register > form > a'),
        $loginUsername = $('#login_username'),

        $registerContainer = $('#register'),
        $registerInsteadLink = $('#login > form > a'),

        $error = $('#error');


    function resetError() {
        $error.hide();
        $error.html('');
    }


    function setError(message) {
        $error.html(message);
        $error.show();
    }


    // Show login page when clicking on 'login' button.
    $loginInsteadLink.on('click', function () {
        resetError();
        $loginContainer.show();
        $registerContainer.hide();
        $loginUsername.focus();
    });


    // Show register page when clicking on 'register' button.
    $registerInsteadLink.on('click', function () {
        resetError();
        $registerContainer.show();
        $loginContainer.hide();
    });


    var $registerUsername = $('#username'),
        $registerPassword = $('#password'),
        $registerPasswordConfirm = $('#password_confirm'),
        $registerFirstName = $('#first_name'),
        $registerLastName = $('#last_name'),
        $registerAge = $('#age');

    // Validate register form fields on blur.
    // if the input doesn't match the predicate we will bring back the focus
    // to that specific field.
    $registerUsername.blur(function() {
        if ($registerUsername.val().length < 2 || $registerUsername.val().length > 50) {
            setError('Username must be between 2 and 50 characters');
            $registerUsername.focus();
            return;
        }

        // Reset the error container if there was no error.
        resetError();
    });

    $registerPassword.blur(function() {
        if ($registerPassword.val().length === 0) {
            setError('Please choose a password');
            $registerPassword.focus();
            return;
        }

        resetError();
    });

    $registerPasswordConfirm.blur(function() {
        if ($registerPassword.val() === $registerPasswordConfirm.val()) {
            $registerPasswordConfirm.get(0).setCustomValidity('');
        } else {
            $registerPasswordConfirm.get(0).setCustomValidity('Passwords doesn\'t match');
            setError('Passwords doesn\'t match');
            $registerPasswordConfirm.focus();
            return;
        }

        resetError();
    });

    $registerFirstName.blur(function() {
        if ($registerFirstName.val().length < 2 || $registerFirstName.val().length > 50) {
            setError('First name must be between 2 and 50 characters');
            $registerFirstName.focus();
            return;
        }

        resetError();
    });

    $registerLastName.blur(function() {
        if ($registerLastName.val().length < 2 || $registerLastName.val().length > 50) {
            setError('Last name must be between 2 and 50 characters');
            $registerLastName.focus();
            return;
        }

        resetError();
    });

    $registerAge.blur(function() {
        if ($registerAge.val() < 13) {
            setError('You must be older than 13 to register');
            $registerAge.focus();
            return;
        }

        resetError();
    });


    // Handle submitting register & login forms the ajax way,
    // and redirect to new location upon a successful submit.
    $loginContainer.find('form').on('submit', function (e) {
        var $form = $(e.target);
        $form.submitForm(function (result) {
            if (result.success) {
                location.href = result.location;
            }
        });

        return false;
    });

    $registerContainer.find('form').on('submit', function (e) {
        var $form = $(e.target);
        $form.submitForm(function (result) {
            if (result.success) {
                location.href = result.location;
            }
        });

        return false;
    });


    // Focus on username field on page load.
    $loginUsername.focus();
});
