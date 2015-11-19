define(['./Struct', './PDA', './Utils','./Stack'], function(Struct, PDAScope, Utils, Stack){

  eval(Utils.importScope('PDAScope'));
  eval(Utils.importScope('Utils'));

  var DPDARuleBook = Struct('DPDARuleBook', ['rules'])
    .method('next_configuration', function(configuration, character){
      return this.rule_for(configuration, character).follow(configuration);
    })
    .method('rule_for', function(configuration, character){
      for (var i=0, l=this.rules.length; i<l; i++){
        if(this.rules[i].is_applies_to(configuration, character)){
          return this.rules[i];
        }
      }
    })
    .method('is_applies_to', function(configuration, character){
      return !!(this.rule_for(configuration, character));
    })
    .method('follow_free_moves', function(configuration){
      if (this.is_applies_to(configuration, epsilon)){
        return this.follow_free_moves(this.next_configuration(configuration, epsilon));
      }
      else {
        return configuration;
      }
    });

  var DPDA = Struct('DPDA', ['__current_configuration__', 'accept_states', 'rulebook'])
    .method('current_configuration', function(){
      return this.rulebook.follow_free_moves(this.__current_configuration__);
    })
    .method('current_configuration', function(configuration){
      return this.__current_configuration__ = configuration;
    })
    .method('is_accepting', function(){
      var _this = this;
      return this.accept_states.reduce(function(last, configuration){
        return last || equal(_this.current_configuration(), configuration);
      }, false);
    })
    .method('next_configuration', function(character){
      if(this.rulebook.is_applies_to(this.current_configuration(), character)){
        return this.rulebook.next_configuration(
          this.current_configuration(), character);
      }
      else{
        return this.current_configuration().stuck();
      }
    })
    .method('is_stuck', function(){
      return this.current_configuration().is_stuck();
    })
    .method('read_character', function(character){
      this.current_configuration(
        this.next_configuration(character)
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
    

  var DPDADesign = Struct('DPDADesign', ['start_state', 'bottom_charachter', 'accept_states', 'rulebook'])
    .method('is_accepts', function(string){
      return this.to_dpda().read_string(string).is_accepting();
    })
    .method('to_dpda', function(){
      var start_stack = Stack([this.bottom_charachter]);
      var start_configuration = PDAConfiguration(this.start_state, start_stack)
      return DPDA(start_configuration, this.accept_states, this.rulebook);
    });

  return {
    PDAConfiguration: PDAConfiguration,
    PDARule: PDARule,
    DPDARuleBook: DPDARuleBook,
    DPDA: DPDA,
    DPDADesign: DPDADesign,
    epsilon: epsilon,
  }
});
