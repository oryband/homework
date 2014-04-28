// Creating objets (functions are also objects in js).
var A = function () {},
    B = function () {},
    C = function () {},
    D = function () {};

// Objects inherit variables and functions.
B.prototype = A;
C.prototype = B;
D.prototype = C;

// Creating instances of above objects.
var a = new A(),
    b = new B(),
    c = new C(),
    d = new D();

// Test.
console.log(a instanceof A);
console.log(b instanceof B);
console.log(c instanceof C);
console.log(d instanceof D);
