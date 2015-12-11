
var Pattern = require('./dist/Pattern');
var Utils = require('./dist/Utils');
eval(Utils.importScope('Pattern'));

var pattern = Repeat(
  Choose(
    Concatenate(Literal('a'), Literal('b')),
    Literal('a')
  )
)

//console.log(Empty().is_matches(''));
//console.log(Literal('a').is_matches('a'));
var p = pattern;
var s = 'cccc';
console.log(p, s);
console.log(p.is_matches(s));
