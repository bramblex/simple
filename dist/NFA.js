
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

  define(['./Class', './Struct', './Set'], function(Class, Struct, Set){

  var epsilon = null;

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

  var NFARuleBook = Struct('NFARuleBook', ['rules'])
    .method('next_states', function(states, character){
      var this_book = this;
      return Set(states.reduce(function(last, state){
        return last.concat(this_book.follow_rules_for(state, character));
      }, []));
    })
    .method('follow_rules_for', function(state, character){
      return this.rules_for(state, character)
        .map(function(r){return r.follow()});
    })
    .method('rules_for', function(state, character){
      return this.rules.filter(function(rule){
        return rule.is_applies_to(state, character);
      });
    })
    .method('follow_free_moves', function(states){
      var more_states = this.next_states(states, epsilon);
      if (more_states.is_subset(states)){
        return states;
      }
      else{
        return this.follow_free_moves(states.or(more_states));
      }
    })
    .method('alphabet', function(){
      return this.rules
        .map(function(rule){
          return rule.character;
        })
        .reduce(function(last, item){
          if (item === epsilon){
            return last;
          }
          else if (last.indexOf(item) >= 0){
            return last;
          }
          else {
            return last.concat([item]);
          }
        }, []);
    });

  var NFA = Struct('NFA', ['__current_states__', 'accept_states', 'rulebook'])
   // get 
    .method('current_states', function(){
      return this.rulebook.follow_free_moves(this.__current_states__);
    })
    // set
    .method('current_states', function(states){
      this.__current_states__ = states;
      return this;
    })
    .method('is_accepting', function(){
      var _this = this;
      return this.accept_states.reduce(function(last, state){
        return last || _this.current_states().has(state);
      }, false);
    })
    .method('read_character', function(character){
      this.current_states(this.rulebook.next_states(this.current_states(), character));
      return this;
    })
    .method('read_string', function(string){
      for (var i=0,l=string.length; i<l; i++){
        var character = string[i];
        this.read_character(character);
      }
      return this;
    });

  var NFADesign = Struct('NFADesign', ['start_state', 'accept_states', 'rulebook'])
    .method('is_accepts', function(string){
      return this.to_nfa().read_string(string).is_accepting();
    })
    .method('to_nfa', function(){
      return NFA(Set([this.start_state]), this.accept_states, this.rulebook);
    })
    .method('to_nfa', function(current_states){
      return NFA(current_states, this.accept_states, this.rulebook);
    });

  var NFASimulation = Struct('NFASimulation', ['nfa_design'])
    .method('next_state', function(state, character){
      return this.nfa_design.to_nfa(state).read_character(character).current_states();
    })
    .method('rules_for', function(state){
      var _this = this;
      return this.nfa_design.rulebook.alphabet()
        .map(function(character){
          return FARule(state, character, _this.next_state(state, character));
        });
    })
    .method('discover_states_and_rules', function(states){
      var _this = this;
      var rules = states.reduce(function(last, state){
        return last.concat(_this.rules_for(state));
      }, []);
      var more_states = Set(rules.map(function(rule){return rule.follow()}));

      if (more_states.is_subset(states)){
        return [states, rules];
      }
      else {
        return this.discover_states_and_rules(states.or(more_states));
      }
    })
    .method('to_dfa_design', function(){
    });

  return {
    epsilon: epsilon,
    FARule: FARule,
    NFARuleBook: NFARuleBook,
    NFA: NFA,
    NFADesign: NFADesign,
    NFASimulation: NFASimulation,
  }
});


})(this, typeof define !== 'undefined' && define);
