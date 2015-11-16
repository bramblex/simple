
var Simple = require('./dist/Simple');
var Utils = require('./dist/Utils');

eval(Utils.importScope('Simple'));

var statement = Sequence(
  Assign('x', Number(0)),
  While(
    LessThan(Variable('x'), Number(10)),
    Assign('x', Add(Variable('x'), Number(1)))
  )
);

// 操作语义
// 小步语义
Machine(statement, Enviroment()).run();
// 大步语义
console.log(statement.evaluate(Enviroment()));

// 指称语义 => JavaScript
console.log(statement.to_js());
console.log(eval(statement.to_js())(Enviroment()));
