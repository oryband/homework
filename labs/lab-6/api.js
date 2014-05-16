$(document).ready(function () {
    "use strict";
    var id,
        key,
        details;

    function idQuery(client) {
        if (key !== "") {
            details.append(
                $("<div>")
                .append($("<span>").text(key + ": "))
                .append($("<span>").text(client)));
        } else {
            // Create divs with client data.
            $.each(client, function (k, v) {
                details.append(
                    $("<div>")
                    .append($("<span>").text(k + ": "))
                    .append($("<span>").text(v)));
            });
        }
    }

    function emptyQuery(client) {
        $.each(client, function (i, el) {
            details.append(
                $("<div>")
                .append($("<span>").text(el)));
        });
    }

    $("#ask").on("click", function () {
        id = $("#id").val();
        key = $("#key").val();
        details = $("<div>");

        $.ajax({
            url: "http://www.cs.bgu.ac.il/~hayounav/lab6/lookup/" + (id ? id + (key ? "/" + key : "") : ""),
            dataType: "jsonp",
            error: function (e) { console.log(e); },
            success: id ? idQuery : emptyQuery
        });

        $("#client").empty().append(details);
        details.empty();
    });

    $("#remove").on("click", function () {
        id = $("#id").val();

        if (!id) {
            $("#client").empty().text("Enter ID to remove.");
        } else {
            $.ajax({
                url: "http://www.cs.bgu.ac.il/~hayounav/lab6/remove/" + id,
                dataType: "jsonp",
                error: function (e) { console.log(e); },
                success: function (reply) {
                    console.log(reply);
                }
            });
        }
    });
});
