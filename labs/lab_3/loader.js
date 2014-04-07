function load(uri, callback) {
    var script = document.createElement('script');

    script.src = uri;
    script.addEventListener('load', callback);
    document.body.appendChild(script);
}
