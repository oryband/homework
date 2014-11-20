function validateURI(uri) {
    var re = /^(http:(?=\/\/))?.+\..+/;
    return uri.search(re) !== -1 ? false : true;
}
