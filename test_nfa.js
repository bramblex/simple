
var NFAScope = require('./dist/NFA');
var Set = require('./dist/Set');
var Utils = require('./dist/Utils');

eval(Utils.importScope('NFAScope'));

var rulebook = NFARuleBook([
  FARule(1, 'a', 1), FARule(1, epsilon, 2), FARule(1, 'a', 2), 
  FARule(2, 'b', 3), 
  FARule(3, 'b', 1), FARule(3, epsilon, 2),
]);

var nfa_design = NFADesign(1, [3], rulebook);
var simulation = NFASimulation(nfa_design);

var start_state = nfa_design.to_nfa().current_states();

var dfa_design = simulation.to_dfa_design();

console.log(dfa_design);
console.log(dfa_design.is_accepts('bbbabb'));
