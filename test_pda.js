
var NPDAScope = require('./dist/NPDA');
var Struct = require('./dist/Struct');
var Stack = require('./dist/Stack');
var Set = require('./dist/Set');
var Utils = require('./dist/Utils');
eval(Utils.importScope('NPDAScope'));
eval(Utils.importScope('Utils'));

var STUCK_STATE = {};

var PDAConfiguration = Struct('PDAConfiguration', ['state', 'stack', 'op_sequence'])
  .method('stuck', function(){
    return PDAConfiguration(STUCK_STATE, this.stack, this.op_sequence);
  })
  .method('is_stuck', function(){
    return equal(this.state, STUCK_STATE);
  })
  .method('equal', function(other){
    this.op_sequence.equal = function(){return true};
    return NPDAScope.PDAConfiguration.prototype.equal.call(this, other);
  });

var PDARule = Struct('PDARule', ['state', 'character', 'next_state', 'pop_character', 'push_characters'])
  .method('is_applies_to', function(configuration, character){
    return equal(this.state, configuration.state) &&
      equal(this.pop_character, configuration.stack.top()) &&
      equal(this.character, character);
  })
  .method('follow', function(configuration){
    //return PDAConfiguration(this.next_state, this.next_stack(configuration), configuration.pushed_sequence.concat(this.push_characters.slice().reverse()));
    //return this
    //configuration.op_sequence.configuration
    var popped_stack = configuration.stack.pop();
    var popped_item = configuration.stack.top();
    var op_sequence = configuration.op_sequence
      .concat([['pop', popped_item]]);
    var pushed_stack = this.push_characters.reduceRight(function(stack, character){
      op_sequence = op_sequence.concat([['push', character]]);
      return stack.push(character);
    }, popped_stack);
    return PDAConfiguration(this.next_state, pushed_stack, op_sequence);
  });

NPDADesign
  .method('to_npda', function(){
    var start_stack = Stack([this.bottom_charachter]);
    var start_configuration = PDAConfiguration(this.start_state, start_stack, []);
    return NPDA(Set([start_configuration]), this.accept_states, this.rulebook);
  });

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
var npda_design = NPDADesign(1, sym['$'], [3], NPDARulebook(rulebook));
var npda = npda_design.to_npda();

var logConfig = function logConfig(config){
  return render(
    //'state: <%state%> | stack: <%stack%> <%top%>\naccpeted: <%accpeted%>',
    'state: <%state%> | stack: <%stack%> <%top%>',
    {
      state: color('cyan', config.state),
      stack: config.stack.contents.slice(0,-1).map(function(i){return csym[i]}).join(' '),
      top: color('red', csym[config.stack.contents[config.stack.contents.length-1]]),
      //accpeted: config.op_sequence.map(function(i){return i[0]+':'+csym[i[1]]}).join(' ')
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
sliceStr('ccccccccc').map(function(t){return sym[t]}).forEach(function(c){
  console.log();
  console.log('input:', csym[c]);
  npda.read_character(c);
  printState(npda);
});
