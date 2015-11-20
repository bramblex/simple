define(['./Struct', './PDA', './Utils', './Stack', './Set'], function(Struct, PDAScope, Utils, Stack, Set){

  eval(Utils.importScope('PDAScope'));
  eval(Utils.importScope('Utils'));

  var NPDARulebook = Struct('NPDARulebook', ['rules'])
    .method('next_configurations', function(configurations, character){
      var _this = this;
      return Set(
        configurations.reduce(function(arr, config){
          return arr.concat(_this.follow_rules_for(config, character));
        }, [])
      );
    })
    .method('follow_rules_for', function(configuration, character){
      return this.rules_for(configuration, character).map(function(rule){
        return rule.follow(configuration);
      });
    })
    .method('rules_for', function(configuration, character){
      return this.rules.filter(function(rule){
        return rule.is_applies_to(configuration, character);
      });
    })
    .method('follow_free_moves', function(configurations){
      var more_configurations = this.next_configurations(configurations, epsilon);
      if (more_configurations.is_subset(configurations)){
        return configurations;
      }
      else {
        return this.follow_free_moves(configurations.or(more_configurations));
      }
    });

  var NPDA = Struct('NPDA', ['__current_configurations__', 'accept_states', 'rulebook'])
    .method('current_configurations', function(){
      return this.rulebook.follow_free_moves(this.__current_configurations__);
    })
    .method('current_configurations', function(configurations){
      this.__current_configurations__ = configurations;
    })
    .method('is_accepting', function(){
      var _this = this;
      return this.accept_states.reduce(function(last, state){
        return last || _this.current_configurations().reduce(function(last, config){
          return last || equal(state, config.state);
        }, false);
      }, false);
    })
    .method('read_character', function(character){
      this.current_configurations(
        this.rulebook.next_configurations(this.current_configurations(), character)
      );
      return this;
    })
    .method('read_string', function(string){
      var _this = this;
      sliceStr(string).forEach(function(character){
        _this.read_character(character);
      });
      return this;
    });

  var NPDADesign = Struct('NPDADesign', ['start_state', 'bottom_charachter', 'accept_states', 'rulebook'])
    .method('is_accepts', function(string){
      return this.to_npda().read_string(string).is_accepting();
    })
    .method('to_npda', function(){
      var start_stack = Stack([this.bottom_charachter]);
      var start_configuration = PDAConfiguration(this.start_state, start_stack);
      return NPDA(Set([start_configuration]), this.accept_states, this.rulebook);
    });

  return {
    PDAConfiguration: PDAConfiguration,
    PDARule: PDARule,
    NPDARulebook: NPDARulebook,
    NPDA: NPDA,
    NPDADesign: NPDADesign,
    epsilon: epsilon,
  }
});
