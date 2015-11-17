
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

  var Set = Class('Set', Array)
    .method('constructor', function(){})
    .method('constructor', function(array){
      var this_set = this;
      array.forEach(function(item){
        if (!this_set.has(item)){
          this_set.push(item);
        }
      });
    })
    .method('has', function(other_item){
      return this.reduce(function(last, item){
        return last || (other_item === item) || (item && item.equal && item.equal(other_item));
      }, false);
    })
    .method('add', function(item){
      if (this.has(item)){
        return Set(this);
      }
      else {
        return Set(this.slice().concat(item));
      }
    })
    .method('remove', function(item){
      if (this.has(item)){
        return Set(this.filter(function(i){return i !== item}));
      }
      else {
        return Set(this);
      }
    })
    .method('size', function(){
      return this.length;
    })
    .method('equal', function(other_set){
      if (!(other_set instanceof Set)){
        return false;
      }
      else if (this.size() !== other_set.size()){
        return false;
      }
      else{
        var this_set = this.slice().sort();
        var other_set = other_set.slice().sort();
        for (var i=0,l=this.size(); i<l; i++){
          if (typeof this_set[i].equal === 'function'){
            if (!this_set[i].equal(other_set[i])){
              return false;
            }
          }
          else if (this_set[i] !== other_set[i]){
            return false;
          }
        }
      }
      return true;
    })
    .method('is_subset', function(set){
      if (set.size() < this.size()){
        return false;
      }
      return this.reduce(function(last, item){
        return last && set.has(item);
      }, true);
    })
    .method('or', function(set){
      return Set(this.slice().concat(set.slice()));
    })
    .method('inspect', '*', function(depth){
      return 'Set' + Utils.inspect(this.slice().sort(), depth-1);
    });
    
  return Set;
});


})(this, typeof define !== 'undefined' && define);
