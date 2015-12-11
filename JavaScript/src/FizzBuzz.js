define(['./Utils'], function(Utils){
  eval(Utils.importScope('Utils'));

  range(1, 100).map(function(n){
    if ( n % 15 === 0) return ('FizzBuzz');
    else if (n % 3 === 0) return ('Fizz');
    else if (n % 5 === 0) return ('Buzz');
    else return(n);
  })

  var F = function(func){ return eval(uncurry(func)) }

  var to_integer  = function(n){ return n (function(i){ return i+1 }) (0) }
  var to_boolean = function(b){ return b (true) (false) }

  var TRUE = F(function(t, f){ return t })
  var FALSE = F(function(t, f){ return f })
  var IF = F(function(b){ return b })
  var IS_ZERO = F(function(n){ return n (function(_){return FALSE}) (TRUE) })

  var ZERO = F(function(f, s){ return s })
  var ONE = F(function(f, s){ return f (s) })
  var TWO = F(function(f, s){ return f (f (s)) })
  var THREE = F(function(f, s){ return f (f (f (s))) })

  var PAIR = F(function(a, b, f){ return f (a) (b) })
  var LEFT = F(function(p){ return p (TRUE) })
  var RIGTH = F(function(p){ return p (FALSE) })

  var INCREMENT = F(function(n, p, x){ return p (n (p) (x)) })
  var SLIDE = F(function(p){ return PAIR (RIGTH (p)) (INCREMENT (RIGTH (p))) })
  var DECREMENT = F(function(n){ return LEFT( n (SLIDE) (PAIR (ZERO) (ZERO))) })

  var ADD = F(function(m, n){ return n (INCREMENT) (m) })
  var SUBTRACT = F(function(m, n){ return n (DECREMENT) (m) })
  var MULTIPY = F(function(m, n){ return n (ADD (m)) (ZERO) })
  var POWER = F(function(m, n){ return n (MULTIPY (m)) (ONE) })

  var Z = (function(f){ return function(x){ return f (function(y){ return x(x)(y) }) } (function(x){return f (function(y){return x(x)(y)})}) })

  var IS_LESS_OR_EQUAL = F(function(m, n){ return IS_ZERO (SUBTRACT (m) (n)) })
  var MOD = Z (function(f){
    return eval(uncurry(function(m, n){
      return IF (IS_LESS_OR_EQUAL (n) (m))
        (function(_){ return f (SUBTRACT (m) (n)) (n) (_) })
        (m)
    }))
  })

  var 

  //print(to_boolean(IS_ZERO (ZERO)))
  //print(to_integer(
    //LEFT (PAIR (ZERO) (ONE))
  //), to_integer(
    //RIGTH (PAIR (TWO) (THREE))
  //));
  print(to_integer(
    MOD (THREE) (TWO)
  ));
  
});
