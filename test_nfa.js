
var NFAScope = require('./dist/NFA');
var Set = require('./dist/Set');
var Utils = require('./dist/Utils');

eval(Utils.importScope('NFAScope'));

//rulebook = NFARuleBook([
  //FARule(1, 'a', 1), FARule(1, 'b', 1), FARule(1, 'b', 2), 
  //FARule(2, 'a', 3), FARule(2, 'b', 3),
  //FARule(3, 'a', 4), FARule(3, 'b', 4)
//]);

//console.log(rulebook.next_states(Set([1]), 'b'));
//console.log(rulebook.next_states(Set([1,2]), 'a'));
//console.log(rulebook.next_states(Set([1,3]), 'b'));

//console.log(NFA(Set([1]), [4], rulebook).is_accepting());
//console.log(NFA(Set([1,2,4]), [4], rulebook).is_accepting());

//var nfa = NFA(Set([1]), [4], rulebook);
//console.log(nfa.is_accepting());
//nfa.read_character('b');
//console.log(nfa.is_accepting());
//nfa.read_character('a');
//console.log(nfa.is_accepting());
//nfa.read_character('b');
//console.log(nfa.is_accepting());

//var nfa = NFA(Set([1]), [4], rulebook);
//console.log(nfa.is_accepting());

//nfa_design = NFADesign(1, [4], rulebook);
//console.log(nfa_design.to_nfa());
//console.log(nfa_design.is_accepts('bab'));
//console.log(nfa_design.is_accepts('bbbbbb'));
//console.log(nfa_design.is_accepts('bbabb'));
//console.log(rulebook.follow_free_moves(Set([1])));
//nfa_design = NFADesign(1, [2,4], rulebook);
//console.log(nfa_design.is_accepts('aa'));
//console.log(nfa_design.is_accepts('aaa'));
//console.log(nfa_design.is_accepts('aaaaa'));
//console.log(nfa_design.is_accepts('aaaaaa'));

var rulebook = NFARuleBook([
  FARule(1, 'a', 1), FARule(1, epsilon, 2), FARule(1, 'a', 2), 
  FARule(2, 'b', 3), 
  FARule(3, 'b', 1), FARule(3, epsilon, 2),
]);

var nfa_design = NFADesign(1, [3], rulebook);
var simulation = NFASimulation(nfa_design);

//console.log(simulation.next_state(Set([1,2]), 'a'));
//console.log(simulation.next_state(Set([1,2]), 'b'));
//console.log(simulation.next_state(Set([3,2]), 'b'));
//console.log(simulation.discover_states_and_rules(Set([1])));
var start_state = nfa_design.to_nfa().current_states();
console.log(
  Utils.inspect(
    simulation.discover_states_and_rules(Set([start_state]))
  ,5)
);

//var a = Set([start_state]);
//console.log(
  //Utils.inspect(
    //Set([2,3]).equal(Set([2,3]))
  //,5)
//);
