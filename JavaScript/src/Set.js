define(['./Class', './Utils'], function(Class, Utils){

  var Set = Class('Set', Array)
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
        return last || Utils.equal(item, other_item);
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
        return Set(this.filter(function(i){return Utils.equal(i, item)}));
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
      else {
        return Utils.equal.array(this.slice().sort(), other_set.slice().sort());
      }
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
