
var Set = require('./dist/Set');

var number_set = Set([2,4,3]);

console.log(number_set.add(1));
console.log(number_set.equal(Set([2,3,4])));
//console.log(number_set.add(1).equal(Set([4,3,2,1])));
//console.log(number_set2.equal(number_set));
console.log(number_set);
