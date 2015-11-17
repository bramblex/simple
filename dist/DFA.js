
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

  define(['./Class', './Struct'], function(Class, Struct){

  var FARule = Struct('FARule', ['state', 'character', 'next_state'])
    .method('is_applies_to', function(state, character){
      return this.state === state && this.character === character;
    })
    .method('follow', function(){
      return this.next_state;
    })
    .method('inspect', '*', function(){
      return this.render('<%state%> -<%character%>-> <%next_state%>');
    })

  var DFARulebook = Struct('DFARulebook', ['rules'])
    .method('next_state', function(state, character){
      return this.rule_for(state, character).follow();
    })
    .method('rule_for', function(state, character){
      for (var i=0, l=this.rules.length; i<l; i++){
        if(this.rules[i].is_applies_to(state, character)){
          return this.rules[i];
        }
      }
      throw Error();
    });

  var DFA = Struct('DFA', ['current_state', 'accept_states', 'rulebook'])
    .method('is_accepting', function(){
      return this.accept_states.indexOf(this.current_state) >= 0;
    })
    .method('read_character', function(character){
      this.current_state = this.rulebook.next_state(this.current_state, character);
      return this;
    })
    .method('read_string', function(string){
      for (var i=0,l=string.length; i<l; i++){
        var character = string[i];
        this.read_character(character);
      }
      return this;
    });

  var DFADesign = Struct('DFADesign', ['start_state', 'accept_states', 'rulebook'])
    .method('to_dfa', function(){
      return DFA(this.start_state, this.accept_states, this.rulebook);
    })
    .method('is_accepts', function(string){
      return this.to_dfa().read_string(string).is_accepting();
    });

  return {
    FARule: FARule,
    DFARulebook: DFARulebook,
    DFADesign: DFADesign,
    DFA: DFA,
  }
});


})(this, typeof define !== 'undefined' && define);
