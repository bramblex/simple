
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

  eval(Utils.importScope('Utils'));

  var Struct = Class('Struct');

  return function(name, values){
    var NewStruct = Struct.extend(name)

    if (!(values instanceof Array)){
      var values_init = slice(values);
      var values = values_init.map(function(e){return e[0]});
      NewStruct
        .method('constructor', function(){
          var _this = this;
          values_init.map(function(kv){
            _this[kv[0]] = kv[1];
          });
        });
    }
    else{
      var values = values.slice();
    }

    NewStruct
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
        
        var result_str = '#<Struct '+name+'\n'+indent(2, content.join(',\n'))+'>';
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
          var _this = this;
          return values.reduce(function(last, key){
            return last && equal(_this[key], other_struct[key]);
          }, true);
        }
      })
      .method('render', function(str){
        return render(
          str, this,
          function(i){return inspect(i)}
        )
      });

    return NewStruct;
  };

});


})(this, typeof define !== 'undefined' && define);
