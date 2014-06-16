$(document).ready(function () {
    'use strict'; 

    var $loginContainer = $('#login');
    var $loginInsteadLink = $('#register > form > a');
    var $loginUsername = $('#login_username');

    var $registerContainer = $('#register');
    var $registerInsteadLink = $('#login > form > a');

    var $error = $('#error');

    function resetError() {
        $error.hide();
        $error.html('');
    }

    function setError(message) {
        $error.html(message);
        $error.show();
    }

    $loginInsteadLink.on('click', function () {
        resetError();
        $loginContainer.show();
        $registerContainer.hide();
        $loginUsername.focus();
    });

    $registerInsteadLink.on('click', function () {
        resetError();
        $registerContainer.show();
        $loginContainer.hide();
    });

    var $registerUsername = $('#username');
    var $registerPassword = $('#password');
    var $registerPasswordConfirm = $('#password_confirm');
    var $registerFirstName = $('#first_name');
    var $registerLastName = $('#last_name');
    var $registerAge = $('#age');

    // validate register form fields on blur.
    // if the input doesn't match the predicate we will bring back the focus
    // to that specific field.
    $registerUsername.blur(function() {
        if ($registerUsername.val().length < 2 || $registerUsername.val().length > 50) {
            setError('Username must be between 2 and 50 characters');
            $registerUsername.focus();
            return;
        }

        // if there was no error, reset the error container
        resetError();
    });

    $registerPassword.blur(function() {
        if ($registerPassword.val().length === 0) {
            setError('Please choose a password');
            $registerPassword.focus();
            return;
        }

        // if there was no error, reset the error container
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

        // if there was no error, reset the error container
        resetError();
    });


    $registerFirstName.blur(function() {
        if ($registerFirstName.val().length < 2 || $registerFirstName.val().length > 50) {
            setError('First name must be between 2 and 50 characters');
            $registerFirstName.focus();
            return;
        }

        // if there was no error, reset the error container
        resetError();
    });

    $registerLastName.blur(function() {
        if ($registerLastName.val().length < 2 || $registerLastName.val().length > 50) {
            setError('Last name must be between 2 and 50 characters');
            $registerLastName.focus();
            return;
        }

        // if there was no error, reset the error container
        resetError();
    });

    $registerAge.blur(function() {
        if ($registerAge.val() < 13) {
            setError('You must be older than 13 to register');
            $registerAge.focus();
            return;
        }

        // if there was no error, reset the error container
        resetError();
    });

    // handle submitting the forms with AJAX.
    // upon a successful submit, redirect to the new location
    $loginContainer.find('form').on('submit', function (e) {
        var $form = $(e.target);
        $form.submitForm(function (result) {
            // success submitting form!
            if (result.success) {
                location.href = result.location;
            }
        });
        return false;
    });

    $registerContainer.find('form').on('submit', function () {
        var $form = $(e.target);
        $form.submitForm(function (result) {
            // success submitting form!
            if (result.success) {
                location.href = result.location;
            }
        });
        return false;
    });

    // we should focus on the username field when the page loads
    $loginUsername.focus();
});
