function appendImage(uri) {
    var img = document.createElement('img');
    img.src = uri;
    document.getElementById('playground').appendChild(img);
}

function imageCallback() {
    appendImage('ikea_cat.jpg');
}

function textCallback() {
    var div = document.createElement('div');
    div.innerText = 'event 2 fired.';
    document.getElementById('playground').appendChild(div);
}

function timer(element, time) {
    element.innerText = time;
    if (time - 1 !== 0) {
        window.setTimeout(timer, 1000, element, time - 1);
    } else {
        element.innerText = '';
        imageCallback();
    }
}

function timerCallback() {
    var div = document.createElement('div');
    document.getElementById('playground').appendChild(div);
    window.setTimeout(timer, 1000, div, 5);
}

function buttonCallback() {
    var playground = document.getElementById('playground'),
        button1 = document.createElement('button'),
        button2 = document.createElement('button');

    button1.id = 'button1';
    button1.type = 'button';
    button1.innerText = 'Click me!';
    button1.addEventListener('click', imageCallback);
    button1.addEventListener('click', textCallback);
    playground.appendChild(button1);

    button2.id = 'button2';
    button2.type = 'button';
    button2.innerText = 'Timer';
    button2.addEventListener('click', timerCallback);
    playground.appendChild(button2);
}

function changeBG(obj) {
    if (this.style) {
        this.style.background = 'black';
    } else if (obj && obj.style) {
        obj.style.background = 'black';
    }
}

function BgCallback() {
    var playground = document.getElementById('playground');
    playground.addEventListener('click', changeBG);
    playground.addEventListener('click', function (e) { changeBG(this); });
}
