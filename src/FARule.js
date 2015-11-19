
define(['./Struct', './Utils'], function(Struct, Utils){
  eval(Utils.importScope('Utils'));

  var FARule = Struct('FARule', ['state', 'character', 'next_state'])
    .method('is_applies_to', function(state, character){
      return equal(this.state, state) && equal(this.character, character);
    })
    .method('follow', function(){
      return this.next_state;
    })
    .method('inspect', '*', function(){
      return this.render('<%state%> -<%character%>-> <%next_state%>');
    });
    return FARule;
});
