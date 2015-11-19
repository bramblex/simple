
(function(__root__, __define__){
  var define = function define(dependencies, factory) {

    var factory = factory || dependencies;
    var dependencies = (Array.isArray(dependencies) && dependencies) || [];

    if ( typeof __define__ === 'function' && __define__.amd){
      __define__(dependencies, factory);
    } else if ( typeof __define__ === 'function' && __define__.cmd){
      __define__(dependencies, function(require, exports, module){
        module.exports = factory.apply(__root__, dependencies.map(function(m){
          return require(m);
        }));
      });
    } else if (typeof exports === 'object'){
      module.exports = factory.apply(__root__, dependencies.map(function(m){
        return require(m);
      }));
    } else{
      var name = document.currentScript.src.replace(/(^.*?)([^\/]+)\.(js)(\?.*$|$)/, '$2');
      name = name.replace('.min', '');
      __root__[name] = factory.apply(__root__, dependencies.map(function(m){
        return __root__[m.replace(/^.*\//, '')];
      }));
    }
  };

  define(['./Struct'], function(Struct){
  var GRAMMAR = [
    { token: 'i', pattern: /if/ },
    { token: 'e', pattern: /else/ },
    { token: 'w', pattern: /while/ },
    { token: 'd', pattern: /do-nothing/ },
    { token: '(', pattern: /\(/ },
    { token: ')', pattern: /\)/ },
    { token: '{', pattern: /\{/ },
    { token: '}', pattern: /\}/ },
    { token: ';', pattern: /;/ },
    { token: '=', pattern: /=/ },
    { token: '+', pattern: /\+/ },
    { token: '*', pattern: /\*/ },
    { token: '<', pattern: /</ },
    { token: 'n', pattern: /[0-9]+/ },
    { token: 'b', pattern: /true|false/ },
    { token: 'v', pattern: /[a-z]+/ }
  ]

  var LexicalAnalyzer = Struct('LexicalAnalyzer', ['string'])
    .method('analyze', function(){
      var tokens = [];
      while (this.has_more_tokens()){
        tokens.push(this.next_token());
      }
      return tokens;
    })
    .method('has_more_tokens', function(){
      return this.string.length > 0;
    })
    .method('next_token', function(){
      var tmp = this.rule_matching(this.string);
      var rule = tmp[0];
      var match = tmp[1];
      this.string = this.string_after(match);
      return rule['token'];
    })
    .method('rule_matching', function(string){
      var matches = GRAMMAR.map(function(item){
      })
    })
});


})(this, typeof define !== 'undefined' && define);
