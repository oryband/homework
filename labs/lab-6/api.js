$(document).ready(function () {
    "use strict";

    $("#ask").on("click", function () {
        var id = $("#id").val(),
            key = $("#key").val();

        if (id) {
            $.ajax({
                url: "http://www.cs.bgu.ac.il/~hayounav/lab6/lookup/" + id + (key ? "/" + key : ""),
                dataType: "jsonp",
                error: function (e) { console.log(e); },
                success: function (client) {

                }
            });
        }
    });
});
