
var DFAScope = require('./dist/DFA');
var Utils = require('./dist/Utils');

eval(Utils.importScope('DFAScope'));

var rulebook = DFARulebook([
  FARule(1, 'a', 2), FARule(1, 'b', 1),
  FARule(2, 'a', 2), FARule(2, 'b', 3),
  FARule(3, 'a', 3), FARule(3, 'b', 3),
]);

var dfa_design = DFADesign(1, [3], rulebook);

console.log(dfa_design.is_accepts('a'));
console.log(dfa_design.is_accepts('baa'));
console.log(dfa_design.is_accepts('baba'));
