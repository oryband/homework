"use strict";

var button = document.getElementById("login-button"),
    user = document.getElementById("user"),
    password = document.getElementById("password");


function validateLogin() {
    button.disabled = user.value !== "admin" || user.value !== password.value;
}

function init() {
    button.disabled = true;
    user.addEventListener("change", validateLogin);
    password.addEventListener("change", validateLogin);
}

init();
