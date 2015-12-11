
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


})(this, typeof define !== 'undefined' && define);
