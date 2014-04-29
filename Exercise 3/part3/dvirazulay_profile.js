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

$(document).ready(function () {
  "use strict";

  // Generate HTML elements.
  var form = $("#login-form"),
      button = $("#login-button"),
      user = $("#user"),
      password = $("#password"),
      article = $("#main-article"),
      footer = $("#footer-container"),
      calculator = new Calculator();

  function validateLogin() {
    button.prop("disabled", (user.val() !== "admin" || user.val() !== password.val()));

    if (user.val() === "admin" && password.val() !== "admin") {
      password.focus();
    }
  }


  // Clears all page, except header.
  function clearPage() {
    article.empty();
    if (footer) {
      footer.remove();
      footer = null;
    }
  }


  // Builds calculator element.
  function showCalculator() {
    clearPage();

    // Build HTML elements.
    var inputContainer = $("<div />"),
    buttonContainer = $("<div />"),
    screen = $("<input />"),
    inputGroup = $("<div />"),
    input = $("<input />"),
    label = $("<label />"),
    add = $("<button />"),
    multiply = $("<button />"),
    clear = $("<button />"),
    calc = $("<div />");

    inputGroup.append(input);
    inputGroup.append(label);

    inputContainer.append(screen);
    inputContainer.append(inputGroup);

    buttonContainer.append(add);
    buttonContainer.append(multiply);
    buttonContainer.append(clear);

    calc.append(inputContainer);
    calc.append(buttonContainer);

    article.append(calc);

    // Set element attributes, and registers events.
    screen.attr("id", "screen");
    screen.attr("class", "form-control");
    screen.attr("type", "text");
    screen.attr("placeholder", "0");
    screen.disabled = true;

    inputGroup.attr("id", "input-group");
    inputGroup.attr("class", "form-group");

    input.attr("id", "input");
    input.attr("class", "form-control");
    input.attr("type", "text");
    input.attr("placeholder", "0");
    input.on("keydown", function (e) {
      var val = e.keyCode - 48;

      // Accept only 'backspace' and numeric keys.
      if (e.keyCode !== 8 && (val < 0 || val > 9)) {
        e.preventDefault();
        inputGroup.addClass("has-error");
        label.text("Only positive integers are allowed.");
      } else {
        inputGroup.removeClass("has-error");
        label.text("");
      }
    });

    label.attr("id", "input-label");
    label.attr("class", "control-label");

    add.attr("id", "add");
    add.attr("class", "btn btn-primary");
    add.text("add");
    add.on("click", function () {
      calculator.add(parseInt(input.val() || 0));
      refreshCalculator();
    });

    multiply.attr("id", "multiply");
    multiply.attr("class", "btn btn-primary");
    multiply.text("multiply");
    multiply.on("click", function () {
      calculator.multiply(parseInt(input.val() || 0));
      refreshCalculator();
    });

    clear.attr("id", "clear");
    clear.attr("class", "btn btn-primary");
    clear.text("clear");
    clear.on("click", function () {
      calculator.clear();
      refreshCalculator();
    });

    inputContainer.attr("id", "input-container");
    inputContainer.attr("class", "form-inline");

    buttonContainer.attr("id", "button-container");
    buttonContainer.attr("class", "form-inline");

    calc.attr("id", "calculator-container");
  }


  // Runs on 'onload' events, initializes everything.
  function init() {
    user.focus();

    button.prop("disabled", true);
    button.on("click", showCalculator);

    form.on("submit", function () { return false; });

    user.on("keyup", validateLogin);
    password.on("keyup", validateLogin);

    user.on("blur", function () {
      if (user.val() !== "admin") {
        user.focus();
      }
    });

    password.on("blur", function () {
      if (password.val() !== "admin") {
        password.focus();
      }
    });
  }

  function refreshCalculator() {
    var screen = $("#screen"),
        input = $("#input"),
        label = $("#input-label"),
        inputGroup = input.parent();

    // update the screen with the new value
    screen.val(calculator.getScreen());

    // clear input box and error state
    input.val("");
    inputGroup.removeClass("has-error");
    label.text("");
  }

  init();
});
