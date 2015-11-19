
var NPDAScope = require('./dist/NPDA');
var Stack = require('./dist/Stack');
var Set = require('./dist/Set');
var Utils = require('./dist/Utils');
eval(Utils.importScope('NPDAScope'));
eval(Utils.importScope('Utils'));

var uniqueId = uniqueId.own();
var csym = {};
var sym = ['E', 'E1', '|', '*', '(', ')', 'c', 'ε', '$']
  .reduce(function(last, sym){
    var nu = String.fromCharCode(uniqueId()+65);
    csym = merge(csym, kv(nu, sym));
    return merge(last, kv(sym, nu));
  }, {});
sym.t = function t(array){
  return array.map(function(i){return sym[i]});
}

var start_rules = [PDARule(1, epsilon, 2, sym['$'], sym.t(['E', '$']))];

var symbol_rules = [
  // E ::= ( E ) E1 | c E1 | ε
  PDARule(2, epsilon, 2, sym['E'], sym.t(['(', 'E', ')', 'E1'])),
  PDARule(2, epsilon, 2, sym['E'], sym.t(['c', 'E1'])),
  PDARule(2, epsilon, 2, sym['E'], sym.t(['ε'])),

  // E1 ::= '|' E E1  | * E1 | E E1 | ε
  PDARule(2, epsilon, 2, sym['E1'], sym.t(['|', 'E', 'E1'])),
  PDARule(2, epsilon, 2, sym['E1'], sym.t(['*', 'E1'])),
  PDARule(2, epsilon, 2, sym['E1'], sym.t(['E', 'E1'])),
  PDARule(2, epsilon, 2, sym['E1'], sym.t(['ε'])),
];

var token_rules = ['|', '*', 'c', '(', ')'].map(function(t){
  return PDARule(2, sym[t], 2, sym[t], []);
});
var epsilon_rules = [PDARule(2, epsilon, 2, sym['ε'], [])];
var stop_rules = [PDARule(2, epsilon, 3, sym['$'], [sym['$']])];


var rulebook = [start_rules, symbol_rules, token_rules, epsilon_rules, stop_rules].reduce(function(a,b){return a.concat(b)});

//console.log(rulebook);
var npda_design = NPDADesign(1, sym['$'], [3], NPDARuleBook(rulebook));
var npda = npda_design.to_npda();

var logConfig = function logConfig(config){
  return render(
    '<%state%> | <%stack%> <%top%>',
    {
      state: color('cyan', config.state),
      stack: config.stack.contents.slice(0,-1).map(function(i){return csym[i]}).join(' '),
      top: color('red', csym[config.stack.contents[config.stack.contents.length-1]])
    }
  );
};

var logConfigs = function logConfigs(configs){
  return configs.map(function(config){return logConfig(config)}).join('\n');
}

var printState = function printState(npda){
  console.log(logConfigs(npda.current_configurations()));
  var accepting = npda.is_accepting();
  console.log('accept:', color(accepting && 'green' || 'red', accepting.toString()));
}

printState(npda);
sliceStr('').map(function(t){return sym[t]}).forEach(function(c){
  console.log();
  console.log('input:', csym[c]);
  npda.read_character(c);
  printState(npda);
});
