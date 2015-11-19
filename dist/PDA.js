
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

  var STUCK_STATE = {};
  var PDAConfiguration = Struct('PDAConfiguration', ['state', 'stack'])
    .method('stuck', function(){
      return PDAConfiguration(STUCK_STATE, this.stack);
    })
    .method('is_stuck', function(){
      return equal(this.state, STUCK_STATE);
    });

  var PDARule = Struct('PDARule', ['state', 'character', 'next_state', 'pop_character', 'push_characters'])
    .method('is_applies_to', function(configuration, character){
      return equal(this.state, configuration.state) &&
        equal(this.pop_character, configuration.stack.top()) &&
        equal(this.character, character);
    })
    .method('follow', function(configuration){
      return PDAConfiguration(this.next_state, this.next_stack(configuration));
    })
    .method('next_stack', function(configuration){
      var popped_stack = configuration.stack.pop();
      return this.push_characters.reduceRight(function(stack, character){
        return stack.push(character);
      }, popped_stack);
    })

  return {
    PDAConfiguration: PDAConfiguration,
    PDARule: PDARule,
    epsilon: null,
  }
});


})(this, typeof define !== 'undefined' && define);
