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
