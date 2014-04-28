"use strict";

var form = document.getElementById("login-form"),
    button = document.getElementById("login-button"),
    user = document.getElementById("user"),
    password = document.getElementById("password"),
    article = document.getElementById("main-article"),
    footer = document.getElementById("footer-container");

function validateLogin() {
    button.disabled = user.value !== "admin" || user.value !== password.value;
}


function clearPage() {
    article.innerHTML = "";
    if (footer) {
        footer.parentNode.removeChild(footer);
        footer = null;
    }
}


function showCalculator() {
    clearPage();

    var screen = document.createElement("input"),
        input = document.createElement("input"),
        add = document.createElement("button"),
        multiply = document.createElement("button"),
        calculator = document.createElement("div");

    screen.setAttribute("id", "screen");
    screen.setAttribute("class", "form-control");
    screen.setAttribute("type", "text");
    screen.setAttribute("placeholder", "0");

    input.setAttribute("id", "input");
    input.setAttribute("class", "form-control");
    input.setAttribute("type", "text");
    input.setAttribute("placeholder", "input");

    add.setAttribute("id", "add");
    add.setAttribute("class", "btn btn-primary");
    add.innerText = "add";

    multiply.setAttribute("id", "multiply");
    multiply.setAttribute("class", "btn btn-primary");
    multiply.innerText = "multiply";

    calculator.setAttribute("id", "calculator");
    calculator.setAttribute("class", "form-inline");

    calculator.appendChild(screen);
    calculator.appendChild(input);
    calculator.appendChild(add);
    calculator.appendChild(multiply);

    article.appendChild(calculator);
}


function init() {
    button.disabled = true;
    button.addEventListener("click", showCalculator);

    form.addEventListener("submit", function () { return false; });

    user.addEventListener("keyup", validateLogin);
    password.addEventListener("keyup", validateLogin);
}

init();
