
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

  define(['./Class', './Utils'], function(Class, Utils){

  var Struct = Class('Struct');
  var indent = Utils.indent;
  var inspect = Utils.inspect;

  return function(name, values){
    var values = values.slice();
    var NewStruct = Struct.extend(name)
      .method('constructor', values.length, function(){
        for (var i=0, l=values.length; i<l; i++){
          this[values[i]] = arguments[i];
        }
      })
      .method('inspect', '*', function(){
        var this_struct = this;
        var content = values.map(function(key){
          return key + '=' + inspect(this_struct[key]);
        });
        
        var result_str = '#<Struct '+name+'\n'+indent(2, content.join(',\n'))+'\n>';
        if (result_str.length < 60){
          result_str = '#<Struct '+name+' '+content.join(', ')+'>';
        }
        return result_str;
      })
      .method('equal', function(other_struct){
        if (!other_struct)
          return false;
        else if (this.__class__ !== other_struct.__class__){
          return false;
        }
        else {
          for (var i=0,l=values.length; i<l; i++){
            var this_value = this[values[i]];
            var other_struct_value = other_struct[values[i]];
            if (this_value instanceof Struct && other_struct_value instanceof Struct){
              if (!this_value.equal(other_struct))
                return false;
            }
            else{
              if (this_value !== other_struct_value)
                return false;
            }
          };
        }
        return true;
      })
      .method('render', function(str){
        return Utils.render(
          str, this,
          function(i){return Utils.inspect(i)}
        )
      });

    return NewStruct;
  };

});


})(this, typeof define !== 'undefined' && define);
