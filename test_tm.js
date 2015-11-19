
var TMScope = require('./dist/TM');
var Utils = require('./dist/Utils');
eval(Utils.importScope('Utils'));
eval(Utils.importScope('TMScope'));

var tape = Tape(['1','0','1'], '1', [], '_');

console.log(tape);
console.log(tape.write('a'));
console.log(tape.move_head_left());
console.log(tape.move_head_left().move_head_left());
console.log(tape.move_head_right());
console.log(tape.move_head_right().move_head_right());
