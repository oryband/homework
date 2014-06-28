'use strict';

// Adds button event listeners to all mail elements in page.
document.addEventListener('DOMContentLoaded', function () {
    var composeButton = document.querySelector('button.compose'),
        composeDialog = document.querySelector('div#compose');


    function openComposeDialog () {
        composeDialog.style.display = 'block';
        composeButton.style.display = 'none';
    }

    composeButton.addEventListener('click', openComposeDialog);


    function closeComposeDialog () {
        composeDialog.style.display = 'none';
        composeButton.style.display = 'block';
    }


    var closeButtons = composeDialog.querySelectorAll('.close_dialog');
    for (var i=0; i < closeButtons.length; i++) {
        closeButtons[i].addEventListener('click', closeComposeDialog);
    }


    // Add event triggers for each mail element.
    function forEachEmailElement(element) {
        var contentElement = element.nextElementSibling,
            isContentShown = false;

        // Toggle mail as read/unread.
        function toggleContent () {
            element.className = element.className.replace('unread', '');
            isContentShown = !isContentShown;
            contentElement.style.display = isContentShown ? 'table-row' : 'none';
        }

        function hideElement () {
            element.remove();
            contentElement.remove();
        }

        function replyToElement (e) {
            composeDialog.querySelector('#recipient').value = 'John Doe';
            composeDialog.querySelector('#subject').value = '';
            composeDialog.querySelector('#body').value = '';
            openComposeDialog();

            // Stop event from bubbling up.
            var evt = e ? e : window.event;
            if (evt.stopPropagation)    evt.stopPropagation();
            if (evt.cancelBubble != null) evt.cancelBubble = true;
            return false;
        }

        element.addEventListener('click', toggleContent);
        contentElement.querySelector('button.close').addEventListener('click', toggleContent);
        element.querySelector('a.delete').addEventListener('click', hideElement);
        element.querySelector('a.reply').addEventListener('click', replyToElement);
    }


    // Attach event listeners to all mail elements.
    var emails = document.querySelectorAll('tr.email');
    for (i=0; i < emails.length; i++) {
        forEachEmailElement(emails[i]);
    }


    // Retry sending unsent (unsuccesful) mails.
    (function retrySendMails () {
        // Fetch and clear unsent mails data structure from localStorage.
        var unsentMails = JSON.parse(localStorage.getItem('unsentMails'));
        localStorage.setItem('unsentMails', JSON.stringify({}));

        // Iterate all mails and retry sending.
        if (!_.isEmpty(unsentMails)) {
            _.each(unsentMails, function (mail, url) {
                $().sendPost(url, mail);
            });
        }

        // Retry sending unsent mails every minute.
        _.delay(retrySendMails, RETRY_DELAY);
    })();
});
