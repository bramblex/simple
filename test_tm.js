
var TMScope = require('./dist/TM');
var Utils = require('./dist/Utils');
eval(Utils.importScope('Utils'));
eval(Utils.importScope('TMScope'));

//var tape = Tape(['1','0','1'], '1', [], '_');

//console.log(tape);
//console.log(tape.write('a'));
//console.log(tape.move_head_left());
//console.log(tape.move_head_left().move_head_left());
//console.log(tape.move_head_right());
//console.log(tape.move_head_right().move_head_right());

var tape =Tape(['1', '0', '1'], '1', [], '_');

//var tape =Tape(['1', '2', '1'], '1', [], '_');

var rulebook = DTMRulebook([
  TMRule(1, '0', 2, '1', 'right'),
  TMRule(1, '1', 1, '0', 'left'),
  TMRule(1, '_', 2, '1', 'right'),
  TMRule(2, '0', 2, '0', 'right'),
  TMRule(2, '1', 2, '1', 'right'),
  TMRule(2, '_', 3, '_', 'left')
]);

var dtm = DTM(TMConfiguration(1, tape), [3], rulebook);

while(!dtm.is_accepting()){
  console.log(dtm.current_configuration, dtm.is_accepting());
  dtm.step();
}
console.log(dtm.current_configuration, dtm.is_accepting());
//dtm.run();
