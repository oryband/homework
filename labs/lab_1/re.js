function validateURI(uri) {
    var re = /^(http:(?=\/\/))?.+\..+/;
    return !!! uri.search(re);
}
