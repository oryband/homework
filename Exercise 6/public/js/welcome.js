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

$(document).ready(function () {
  'use strict'; 

  var $loginContainer = $('#login');
  var $loginInsteadLink = $('#register > form > a');
  var $loginUsername = $('#login_username');

  var $registerContainer = $('#register');
  var $registerInsteadLink = $('#login > form > a');

  var $error = $('#error');

  $.fn.submitForm = function(callback) {
    resetError();

    $.post(
      this.attr('action'), 
      this.serializeObject(),
      function (result) {
        if (result.error) {
          var $error = $('#error');
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

  function resetError() {
    $error.hide();
    $error.html('');
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
