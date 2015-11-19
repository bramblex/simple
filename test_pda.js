
var NPDAScope = require('./dist/NPDA');
var Stack = require('./dist/Stack');
var Set = require('./dist/Set');
var Utils = require('./dist/Utils');
eval(Utils.importScope('NPDAScope'));
eval(Utils.importScope('Utils'));

var rulebook = NPDARuleBook([
  PDARule(1, 'a', 1, '$', ['a', '$']),
  PDARule(1, 'a', 1, 'a', ['a', 'a']),
  PDARule(1, 'a', 1, 'b', ['a', 'b']),
  PDARule(1, 'b', 1, '$', ['b', '$']),
  PDARule(1, 'b', 1, 'a', ['b', 'a']),
  PDARule(1, 'b', 1, 'b', ['b', 'b']),
  PDARule(1, epsilon, 2, '$', ['$']),
  PDARule(1, epsilon, 2, 'a', ['a']),
  PDARule(1, epsilon, 2, 'b', ['b']),
  PDARule(2, 'a', 2, 'a', []),
  PDARule(2, 'b', 2, 'b', []),
  PDARule(2, epsilon, 3, '$', ['$'])
]);

var npda_design = NPDADesign(1, '$', [3], rulebook);

console.log(npda_design.is_accepts('abba'));
