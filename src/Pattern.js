define(['./Struct', './Utils', './NFA'], function(__Struct__, Utils, NFAScope){

  eval(Utils.importScope('NFAScope'));

  var Struct = function(name, values){
    var Pattern = __Struct__(name, values).extend(name);
    Pattern 
      .method('render', function(str){
        return Utils.render(
          str, Utils.merge(this, Utils.kv('this', this)),
          function(i){return i.to_s && i.to_s() || i;}
        )
      })
      .method('bracket', function(outer_precedence){
        if (this.precedence() < outer_precedence()){
          return this.render('(<%this%>)');
        }
        else {
          return this.to_s();
        }
      })
      .method('inspect', '*', function(){
        return this.render('/<%this%>/')
      })
      .method('is_matches', function(string){
        return this.to_nfa_design().is_accepts(string);
      });
    return Pattern;
  };

  var Empty = Struct('Empty', [])
    .method('to_s', function(){
      return ''
    })
    .method('precedence', function(){
      return 3;
    })
    .method('to_nfa_design', function(){
      var start_state = Utils.uniqueId();
      var accept_states = [start_state];
      var rulebook = NFARuleBook([]);
      return NFADesign(start_state, accept_states, rulebook);
    });

  var Literal = Struct('Literal', ['character'])
    .method('to_s', function(){
      return this.character;
    })
    .method('precedence', function(){
      return 3;
    })
    .method('to_nfa_design', function(){
      var start_state = Utils.uniqueId();
      var accept_state = Utils.uniqueId();
      var rulebook = NFARuleBook([
        FARule(start_state, this.character, accept_state)
      ]);
      return NFADesign(start_state, [accept_state], rulebook);
    });


  var Concatenate = Struct('Concatenate', ['first', 'second'])
    .method('to_s', function(){
      var _this = this;
      return [this.first, this.second].map(function(pattern){ return pattern.bracket(_this.precedence) }).join('');
    })
    .method('precedence', function(){
      return 1;
    })
    .method('to_nfa_design', function(){
      var first_nfa_design = this.first.to_nfa_design();
      var second_nfa_design = this.second.to_nfa_design();

      var start_state = first_nfa_design.start_state;
      var accept_states = second_nfa_design.accept_states;
      var rules = first_nfa_design.rulebook.rules.concat(second_nfa_design.rulebook.rules);
      var extra_rules = first_nfa_design.accept_states.map(function(state){
        return FARule(state, epsilon, second_nfa_design.start_state);
      });
      var rulebook = NFARuleBook(rules.concat(extra_rules));
      return NFADesign(start_state, accept_states, rulebook);
    });

  var Choose = Struct('Choose', ['first', 'second'])
    .method('to_s', function(){
      var _this = this;
      return [this.first, this.second]
        .map(function(pattern){ return pattern.bracket(_this.precedence) })
        .join('|');
    })
    .method('precedence', function(){
      return 0;
    })
    .method('to_nfa_design', function(){
      var first_nfa_design = this.first.to_nfa_design();
      var second_nfa_design = this.second.to_nfa_design();

      var start_state = Utils.uniqueId();
      var accept_states = first_nfa_design.accept_states
        .concat(second_nfa_design.accept_states);
      var rules = first_nfa_design.rulebook.rules.concat(second_nfa_design.rulebook.rules);
      var extra_rules = [first_nfa_design, second_nfa_design]
        .map(function(nfa_design){
          return FARule(start_state, epsilon, nfa_design.start_state);
        });
      var rulebook = NFARuleBook(rules.concat(extra_rules));
      return NFADesign(start_state, accept_states, rulebook);
    });


  var Repeat = Struct('Repeat', ['pattern'])
    .method('to_s', function(){
      return this.pattern.bracket(this.precedence) + '*';
    })
    .method('precedence', function(){
      return 2;
    })
    .method('to_nfa_design', function(){
      var pattern_design = this.pattern.to_nfa_design();

      var start_state = Utils.uniqueId();
      var accept_states = pattern_design.accept_states.concat([start_state]);
      var rules = pattern_design.rulebook.rules;
      var extra_rules = pattern_design.accept_states
        .map(function(accept_state){
          return FARule(accept_state, epsilon, pattern_design.start_state);
        })
        .concat([FARule(start_state, epsilon, pattern_design.start_state)]);
      var rulebook = NFARuleBook(rules.concat(extra_rules));
      return NFADesign(start_state, accept_states, rulebook);
    });

  return {
    Empty: Empty,
    Literal: Literal,
    Concatenate: Concatenate,
    Choose: Choose,
    Repeat: Repeat,
  }
});
