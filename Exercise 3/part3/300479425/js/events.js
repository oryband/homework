$(document).ready(function () {
    "use strict";

    // Function globals (members).
    var form = $("#login-form"),
        button = $("#login-button"),
        user = $("#user"),
        password = $("#password");


    // Clears all page, except header.
    function clearPage() {
        $("#main-article").empty();
        $("footer").each(function () { $(this).remove(); });
    }


    // Builds calculator element.
    function showCalculator() {
        clearPage();

        // Build HTML elements.
        var screen = $("<input>", { "id": "screen", "class": "form-control", "type": "text", "placeholder": "0", "disabled": "true" }),
            input = $("<input>", { "id": "input", "class": "form-control", "type": "text", "placeholder": "0" }),
            label = $("<label>", { "id": "input-label", "class": "control-label" }),
            add = $("<button>", { "id": "add", "class": "btn btn-primary" }).text("add"),
            multiply = $("<button>", { "id": "multiply", "class": "btn btn-primary" }).text("multiply"),
            clear = $("<button>", { "id": "clear", "class": "btn btn-primary" }).text("clear"),

            inputGroup = $("<div>", { "id": "input-group", "class": "form-group" }).append(input, label),

            inputContainer = $("<div>", { "id": "input-container", "class": "form-inline" }).append(screen, inputGroup),
            buttonContainer = $("<div>", { "id": "button-container", "class": "form-inline" }).append(add, multiply, clear),

            calc = $("<div>", { "id": "calculator" }).append(inputContainer, buttonContainer),

            calculator = new Calculator(),

            updateScreen = function () {
                screen.val("" + calculator.getScreen());
                input.val("");
                inputGroup.removeClass("has-error");
                label.text("");
            };

        // Append to body.
        $("#main-article").append(calc);

        // Register events.
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

        add.on("click", function () {
            calculator.add(parseInt(input.val() || 0));
            updateScreen();
        });

        multiply.on("click", function () {
            calculator.multiply(parseInt(input.val() || 0));
            updateScreen();
        });

        clear.on("click", function () {
            calculator.clear();
            updateScreen();
        });
    }


    // Focuses on login inputs, and doesn't allow blurring until correct
    // user and password have been given.
    function focusLogin() {
        var validateLogin = function () {
            button.prop("disabled",
                    (user.val() !== "admin" || user.val() !== password.val()));
        };

        user.on("keyup", validateLogin);
        password.on("keyup", validateLogin);

        user.focus();

        user.on("blur keyup", function () {
            if (user.val() !== "admin") {
                user.focus();
            } else {
                password.focus();
            }
        });

        password.on("blur", function () {
            if (password.val() !== "admin") {
                password.focus();
            }
        });
    }


    // Toggles ID/username on click.
    function toggleIdOnClick() {
        $("#id").on("click", function () {
            if ($(this).html() === "300479425") {
                $(this).html("oryband");
            } else {
                $(this).html("300479425");
            }
        });
    }


    // Shows/hides paragraphs on click.
    function toggleParagraphsOnClick() {
        $("#par-1-title").on("click", function () {
            $("#par-1").toggle();
        });

        $("#par-2-title").on("click", function () {
            $("#par-2").toggle();
        });
    }


    // Adds a footer quote on click.
    function addQuoteOnClick() {
        $("#jack-quote").on("click", function () {
            $("#footer").append(
                $("<blockquote>", { "cite": "http://how-i-met-your-mother.wikia.com/wiki/Legendary" })
                    .append($("<p>").html("Legen... wait for it...")
                            // Show rest of quote after a 3 sec delay.
                        .append($("<span>", { "id": "dary" }).text(" dary. LEGENDARY!").delay(3000).fadeIn(200),
                                $("<footer>")
                                    .append($("<cite>", { "title": "Barney Stinson" })
                                        .text("Barney Stinson"))))

                    // Clear quote on click.
                    .on("click", function () { $(this).remove(); }));
        });
    }


    // Runs on 'onload' events, initializes everything.
    function init() {
        form.on("submit", function () { return false; });
        button.prop("disabled", true).on("click", showCalculator);

        // Part 3 additions. Function names are self-documentary.
        focusLogin();
        toggleIdOnClick();
        toggleParagraphsOnClick();
        addQuoteOnClick();
    }

    init();
});
