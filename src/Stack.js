define(['./Struct', './Utils'], function(Struct, Utils){
  eval(Utils.importScope('Utils'));

  var Stack = Struct('Stack', {'contents': []})
    .method('push', function(character){
      return Stack(this.contents.concat([character]));
    })
    .method('pop', function(){
      return Stack(this.contents.slice(0,-1))
    })
    .method('top', function(){
      return this.contents[this.contents.length-1];
    })
    .method('size', function(){
      return this.contents.length;
    })
    .method('inspect', '*', function(){
      return render(
        '#<Stack <%contents%><%top%>>',
        {contents: this.contents.slice(0,-1).join(''),
          top: color('red', this.top())}
      );
    })
    .method('equal', function(other_stack){
      if (!(other_stack instanceof Stack)){
        return false;
      }
      else {
        return equal(this.contents, other_stack.contents);
      }
    });

  return Stack;
});
