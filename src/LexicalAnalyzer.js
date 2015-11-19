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
