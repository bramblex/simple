
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


})(this, typeof define !== 'undefined' && define);
