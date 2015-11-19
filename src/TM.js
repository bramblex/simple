define(['./Struct', './Utils'], function(Struct, Utils){
  eval(Utils.importScope('Utils'));

  var Tape = Struct('Tape', {left: [], middle: '_', right: [], blank: '_'})
    .method('inspect', '*', function(){
      return render(
        '#<Tape <%left%>(<%middle%>)<%right%>>',
        {left: this.left.join(''), middle: this.middle, right: this.right.join('')}
      );
    })
    .method('write', function(character){
      return Tape(this.left, character, this.right, this.blank);
    })
    .method('move_head_left', function(){
      return Tape(
        this.left.slice(0, -1),
        this.left[this.left.length-1] || this.blank,
        [this.middle].concat(this.right),
        this.blank
      );
    })
    .method('move_head_right', function(){
      return Tape(
        this.left.concat([this.middle]),
        this.right[0] || this.blank,
        this.right.slice(1),
        this.blank
      );
    });

  var TMConfiguration = Struct('TMConfiguration', ['state', 'tape']);

  var TMRule = Struct('TMRule', ['state', 'character', 'next_state', 'write_character', 'direction'])

  return {
    Tape: Tape
  }
});
