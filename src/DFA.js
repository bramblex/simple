define(['./Class', './Struct'], function(Class, Struct){

  var FARule = Struct('FARule', ['state', 'character', 'next_state'])
    .method('applies_to', function(state, character){
      return this.state === state && this.character === character;
    })
    .method('follow', function(){
      return this.next_state;
    })
    .method('inspect', '*', function(){
      return this.render('<%state%> --<%character%>--> <%next_state%>');
    })

  var DFARulebook = Struct('DFARulebook', ['rules'])
    .method('next_state', function(state, character){
      return this.rule_for(state, character).follow();
    })
    .method('rule_for', function(state, character){
      for (var i=0, l=this.rules.length; i<l; i++){
        if(this.rules[i].applies_to(state, character)){
          return this.rules[i];
        }
      }
      throw Error();
    });

  return {
    FARule: FARule,
    DFARulebook: DFARulebook
  }
});
