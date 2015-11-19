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
