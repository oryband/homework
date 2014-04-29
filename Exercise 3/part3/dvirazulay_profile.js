"use strict";

var Calculator = function () {
    // Private.
    var screen = 0;

    this.getScreen = function () {
        return screen;
    };

    // We assume valid value was given.
    this.add = function (value) {
        screen += value;
    };

    this.multiply = function (value) {
        screen *= value;
    };

    this.clear = function () {
        screen = 0;
    };
};

window.onload = (function () {
    "use strict";

    // Generate HTML elements.
    var form = document.getElementById("login-form"),
        button = document.getElementById("login-button"),
        user = document.getElementById("user"),
        password = document.getElementById("password"),
        article = document.getElementById("main-article"),
        footer = document.getElementById("footer-container"),
        calculator = new Calculator();


    function validateLogin() {
        button.disabled = user.value !== "admin" || user.value !== password.value;
    }


    // Clears all page, except header.
    function clearPage() {
        article.innerHTML = "";
        if (footer) {
            footer.parentNode.removeChild(footer);
            footer = null;
        }
    }


    // Builds calculator element.
    function showCalculator() {
        clearPage();

        // Build HTML elements.
        var inputContainer = document.createElement("div"),
            buttonContainer = document.createElement("div"),
            screen = document.createElement("input"),
            inputGroup = document.createElement("div"),
            input = document.createElement("input"),
            label = document.createElement("label"),
            add = document.createElement("button"),
            multiply = document.createElement("button"),
            clear = document.createElement("button"),
            calc = document.createElement("div");

        inputGroup.appendChild(input);
        inputGroup.appendChild(label);

        inputContainer.appendChild(screen);
        inputContainer.appendChild(inputGroup);

        buttonContainer.appendChild(add);
        buttonContainer.appendChild(multiply);
        buttonContainer.appendChild(clear);

        calc.appendChild(inputContainer);
        calc.appendChild(buttonContainer);

        article.appendChild(calc);

        // Set element attributes, and registers events.
        screen.setAttribute("id", "screen");
        screen.setAttribute("class", "form-control");
        screen.setAttribute("type", "text");
        screen.setAttribute("placeholder", "0");
        screen.disabled = true;

        inputGroup.setAttribute("id", "input-group");
        inputGroup.setAttribute("class", "form-group");

        input.setAttribute("id", "input");
        input.setAttribute("class", "form-control");
        input.setAttribute("type", "text");
        input.setAttribute("placeholder", "0");
        input.addEventListener("keydown", function (e) {
            var val = e.keyCode - 48;

            // Accept only 'backspace' and numeric keys.
            if (e.keyCode !== 8 && (val < 0 || val > 9)) {
                e.preventDefault();
                inputGroup.className += " has-error";
                label.innerText = "Only positive integers are allowed.";
            } else {
                inputGroup.className = "form-group";
                label.innerText = "";
            }
        });

        label.setAttribute("id", "input-label");
        label.setAttribute("class", "control-label");

        add.setAttribute("id", "add");
        add.setAttribute("class", "btn btn-primary");
        add.innerText = "add";
        add.addEventListener("click", function () {
            calculator.add(parseInt(input.value || 0));
            screen.value = calculator.getScreen();
            input.value = "";
            inputGroup.className = "form-group";
            label.innerText = "";
        });

        multiply.setAttribute("id", "multiply");
        multiply.setAttribute("class", "btn btn-primary");
        multiply.innerText = "multiply";
        multiply.addEventListener("click", function () {
            calculator.multiply(parseInt(input.value || 0));
            screen.value = calculator.getScreen();
            input.value = "";
            inputGroup.className = "form-group";
            label.innerText = "";
        });

        clear.setAttribute("id", "clear");
        clear.setAttribute("class", "btn btn-primary");
        clear.innerText = "clear";
        clear.addEventListener("click", function () {
            calculator.clear();
            screen.value = calculator.getScreen();
            input.value = "";
            inputGroup.className = "form-group";
            label.innerText = "";
        });

        inputContainer.setAttribute("id", "input-container");
        inputContainer.setAttribute("class", "form-inline");

        buttonContainer.setAttribute("id", "button-container");
        buttonContainer.setAttribute("class", "form-inline");

        calc.setAttribute("id", "calculator");
    }


    // Runs on 'onload' events, initializes everything.
    function init() {
        button.disabled = true;
        button.addEventListener("click", showCalculator);

        form.addEventListener("submit", function () { return false; });

        user.addEventListener("keyup", validateLogin);
        password.addEventListener("keyup", validateLogin);
    }

    init();
});
