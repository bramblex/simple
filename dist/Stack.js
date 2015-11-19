
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

  define(['./Struct', './Utils'], function(Struct, Utils){

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
      return this.contents.length();
    })
    .method('inspect', '*', function(){
      return Utils.render(
        '#<Stack <%contents%>(<%top%>)>',
        {contents: this.contents.slice(0,-1).join(''), top: this.top()}
      );
    })
    .method('equal', function(other_stack){
      if (!(other_stack instanceof Stack)){
        return false;
      }
      else {
        return Utils.equal(this.contents, other_stack.contents);
      }
    });

  return Stack;
});


})(this, typeof define !== 'undefined' && define);
