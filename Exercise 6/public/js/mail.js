document.addEventListener('DOMContentLoaded', function (e) {
  var composeButton = document.querySelector('button.compose');
  var composeDialog = document.querySelector('div#compose');
  composeButton.addEventListener('click', openComposeDialog);

  var openComposeDialog = function (e) {
    composeDialog.style.display = 'block'; 
    composeButton.style.display = 'none';
  };
  var closeComposeDialog = function (e) {
    composeDialog.style.display = 'none'; 
    composeButton.style.display = 'block';
  };

  var closeButtons = composeDialog.querySelectorAll('.close_dialog');
  for (var i = 0; i < closeButtons.length; ++i) {
    closeButtons[i].addEventListener('click', closeComposeDialog);
  }

  function forEachEmailElement(element) {
    var contentElement = element.nextElementSibling;
    var isContentShown = false;
    var toggleContent = function (e) {
      element.className = element.className.replace('unread', '');
      isContentShown = !isContentShown;
      contentElement.style.display = isContentShown ? 'table-row' : 'none';
    };
    var hideElement = function (e) {
      element.remove();
      contentElement.remove();
    };
    var replyToElement = function (e) {
      composeDialog.querySelector('#recipient').value = 'John Doe';
      composeDialog.querySelector('#subject').value = '';
      composeDialog.querySelector('#body').value = '';
      openComposeDialog();

      // stop event from bubbling up
      var evt = e ? e:window.event;
      if (evt.stopPropagation)    evt.stopPropagation();
      if (evt.cancelBubble!=null) evt.cancelBubble = true;
      return false;
    };

    element.addEventListener('click', toggleContent);
    contentElement.querySelector('button.close').addEventListener('click', toggleContent);
    element.querySelector('a.delete').addEventListener('click', hideElement);
    element.querySelector('a.reply').addEventListener('click', replyToElement);
  }

  var emails = document.querySelectorAll("tr.email");
  for (var i = 0; i < emails.length; ++i) {
    forEachEmailElement(emails[i]);
  }

});
