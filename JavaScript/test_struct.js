
var Struct = require('./dist/Struct');
var Set = require('./dist/Set');
var Stack = require('./dist/Stack');
var Utils = require('./dist/Utils');
var A = Struct('A', ['a', 'b']);

console.log(
  Set([1,2,3]).equal(Set([3,2,1,3,2,1]))
);
