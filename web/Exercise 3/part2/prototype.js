// Creating objets (functions are also objects in js).
var A = function () {},
    B = function () {},
    C = function () {},
    D = function () {};

// Objects inherit variables and functions.
B.prototype = new A();
C.prototype = new B();
D.prototype = new C();

// Creating instances of above objects.
var a = new A(),
    b = new B(),
    c = new C(),
    d = new D();

// Test.
console.log(a instanceof A);

console.log(b instanceof B);
console.log(b instanceof A);

console.log(c instanceof C);
console.log(c instanceof B);
console.log(c instanceof A);

console.log(d instanceof D);
console.log(d instanceof C);
console.log(d instanceof B);
console.log(d instanceof A);
