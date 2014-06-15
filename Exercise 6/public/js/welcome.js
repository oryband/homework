document.addEventListener('DOMContentLoaded', function (e) {
  var loginContainer = document.getElementById('login');
  var loginInsteadLink = document.querySelector('#register > form > a');

  loginInsteadLink.addEventListener('click', function (e) {
    loginContainer.style.display = 'block'; 
    registerContainer.style.display = 'none'; 
  });

  var registerContainer = document.getElementById('register');
  var registerInsteadLink = document.querySelector('#login > form > a');

  registerInsteadLink.addEventListener('click', function (e) {
    registerContainer.style.display = 'block'; 
    loginContainer.style.display = 'none'; 
  });

  var registerPassword = document.getElementById('password');
  var registerPasswordConfirm = document.getElementById('password_confirm');
  var validatePassword = function (e) {
    if (registerPassword.value == registerPasswordConfirm.value) {
      registerPasswordConfirm.setCustomValidity('');
    } else {
      registerPasswordConfirm.setCustomValidity('Passwords doesn\'t match');
    }
  };
  registerPassword.addEventListener('change', validatePassword);
  registerPasswordConfirm.addEventListener('change', validatePassword);
});
