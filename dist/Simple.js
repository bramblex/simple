
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

  define(['./Class', './Struct', './Utils'], function(Class, __Struct__, Utils){

  var Enviroment = Class('Enviroment', Object)
    .method('constructor', function(){
    })
    .method('constructor', function(obj){
      for (var key in obj){
        if (obj.hasOwnProperty(key)){
          this[key] = obj[key];
        }
      }
    })
    .method('merge', function(obj){
      var env = Enviroment(this);
      for (var key in obj){
        if (obj.hasOwnProperty(key)){
          env[key] = obj[key];
        }
      }
      return env;
    })
    .method('merge', function(key, value){
      var env = Enviroment(this);
      env[key] = value;
      return env;
    })
    .method('inspect', '*', function(){
      var env = {}
      for (var key in this){
        if (this.hasOwnProperty(key) && key !== '__class__'){
          env[key] = this[key];
        }
      }
      return env;
    });

  var Struct = function(name, values){
    var NewStruct = __Struct__(name, values).extend(name);
    NewStruct.method('render', function(str){
      return Utils.render(
        str, Utils.merge(this, Utils.kv('this', this)),
        function(i){return i.to_s && i.to_s() || i;}
      )
    });
    return NewStruct;
  };

  var Number = Struct('Number', ['value'])
    .method('to_s', function(){
      return this.value.toString();
    })
    .method('to_js', function(){
      return Utils.render(
        '(function(e){return <%value%>})',
        {value: Utils.inspect(this.value)}
      );
    })
    .method('inspect', '*', function(){
      return this.render('<< <%this%> >>');
    })
    .method('is_reducible', function(){
      return false;
    })
    .method('evaluate', function(enviroment){
      return this;
    });

  var Boolean = Struct('Boolean', ['value'])
    .method('to_s', function(){
      return this.value.toString();
    })
    .method('to_js', function(){
      return Utils.render(
        '(function(e){return <%value%>})',
        {value: Utils.inspect(this.value)}
      );
    })
    .method('inspect', '*', function(){
      return this.render('<< <%this%> >>');
    })
    .method('is_reducible', function(){
      return false;
    })
    .method('evaluate', function(enviroment){
      return this;
    });

  var Variable = Struct('Variable', ['name'])
    .method('to_s', function(){
      return this.name.toString();
    })
    .method('to_js', function(){
      return Utils.render(
        '(function(e){return e[<%name%>]})',
        {name: Utils.inspect(this.name)}
      );
    })
    .method('inspect', '*', function(){
      return this.render('<< <%this%> >>');
    })
    .method('is_reducible', function(){
      return true;
    })
    .method('reduce', function(enviroment){
      return enviroment[this.name];
    })
    .method('evaluate', function(enviroment){
      return enviroment[this.name];
    });

  var Add = Struct('Add', ['left', 'right'])
    .method('to_s', function(){
      return this.render('(<%left%> + <%right%>)');
    })
    .method('to_js', function(){
      return Utils.render(
        '(function(e){return <%left%>(e) + <%right%>(e)})',
        {left: this.left.to_js(), right: this.right.to_js()}
      );
    })
    .method('inspect', '*', function(){
      return this.render('<< <%this%> >>');
    })
    .method('is_reducible', function(){
      return true;
    })
    .method('reduce', function(enviroment){
      if (this.left.is_reducible()){
        return Add(this.left.reduce(enviroment), this.right);
      }
      else if (this.right.is_reducible()){
        return Add(this.left, this.right.reduce(enviroment));
      }
      else {
        return Number(this.left.value + this.right.value);
      }
    })
    .method('evaluate', function(enviroment){
      return Number(
        this.left.evaluate(enviroment).value + this.right.evaluate(enviroment).value
      );
    });

  var Multipy = Struct('Multipy', ['left', 'right'])
    .method('to_s', function(){
      return this.render('(<%left%> * <%right%>)');
    })
    .method('to_js', function(){
      return Utils.render(
        '(function(e){return <%left%>(e) * <%right%>(e)})',
        {left: this.left.to_js(), right: this.right.to_js()}
      );
    })
    .method('inspect', '*', function(){
      return this.render('<< <%this%> >>');
    })
    .method('is_reducible', function(){
      return true;
    })
    .method('reduce', function(enviroment){
      if (this.left.is_reducible()){
        return Multipy(this.left.reduce(enviroment), this.right);
      }
      else if (this.right.is_reducible()){
        return Multipy(this.left, this.right.reduce(enviroment));
      }
      else {
        return Number(this.left.value * this.right.value);
      }
    })
    .method('evaluate', function(enviroment){
      return Number(
        this.left.evaluate(enviroment).value * this.right.evaluate(enviroment).value
      );
    });

  var LessThan = Struct('LessThan', ['left', 'right'])
    .method('to_s', function(){
      return this.render('(<%left%> < <%right%>)');
    })
    .method('to_js', function(){
      return Utils.render(
        '(function(e){return <%left%>(e) < <%right%>(e)})',
        {left: this.left.to_js(), right: this.right.to_js()}
      );
    })
    .method('inspect', '*', function(){
      return this.render('<< <%this%> >>');
    })
    .method('is_reducible', function(){
      return true;
    })
    .method('reduce', function(enviroment){
      if (this.left.is_reducible()){
        return LessThan(this.left.reduce(enviroment), this.right);
      }
      else if (this.right.is_reducible()){
        return LessThan(this.left, this.right.reduce(enviroment));
      }
      else {
        return Boolean(this.left.value < this.right.value);
      }
    })
    .method('evaluate', function(enviroment){
      return Boolean(
        this.left.evaluate(enviroment).value < this.right.evaluate(enviroment).value
      );
    });

  var DoNothing = Struct('DoNothing', [])
    .method('to_s', function(){
      return 'do-nothing'
    })
    .method('to_js', function(){
      return '(function(e){return e})';
    })
    .method('inspect', '*', function(){
      return this.render('<< <%this%> >>');
    })
    .method('is_reducible', function(){
      return false;
    })
    .method('evaluate', function(enviroment){
      return enviroment;
    });

  var Assign = Struct('Assign', ['name', 'expression'])
    .method('to_s', function(){
      return this.render('<%name%> = <%expression%>');
    })
    .method('to_js', function(){
      return Utils.render(
        '(function(e){return e.merge({<%name%>: <%expression%>(e)})})',
        {name: this.name, expression: this.expression.to_js()}
      );
    })
    .method('inspect', '*', function(){
      return this.render('<< <%this%> >>');
    })
    .method('is_reducible', function(){return true})
    .method('reduce', function(enviroment){
      if (this.expression.is_reducible()){
        return [
          Assign(this.name, this.expression.reduce(enviroment)),
          enviroment];
      }
      else {
        return [
          DoNothing(),
          enviroment.merge(this.name, this.expression)
        ];
      }
    })
    .method('evaluate', function(enviroment){
      return enviroment.merge(this.name, this.expression.evaluate(enviroment));
    });

  var If = Struct('If', ['condition', 'consequence', 'alternative'])
    .method('to_s', function(){
      return this.render(
        'if (<%condition%>) { <%consequence%> } else { <%alternative%> }'
      )
    })
    .method('to_js', function(){
      return Utils.render(
        '(function(e){ if (<%condition%>(e)) \
        {return <%consequence%>(e)} \
        else {return <%alternative%>(e)}})',
        { condition: this.condition.to_js(),
          consequence: this.consequence.to_js(),
          alternative: this.alternative.to_js()}
      );
    })
    .method('inspect', '*', function(){
      return this.render('<< <%this%> >>');
    })
    .method('is_reducible', function(){return true;})
    .method('reduce', function(enviroment){
      if (this.condition.is_reducible()){
        return [
          If(this.condition.reduce(enviroment), this.consequence, this.alternative),
          enviroment
        ]
      }
      else{
        if (this.condition.equal(Boolean(true))){
          return [this.consequence, enviroment];
        }
        else if (this.condition.equal(Boolean(false))){
          return [this.alternative, enviroment];
        }
      }
    })
    .method('evaluate', function(enviroment){
      var b = this.condition.evaluate(enviroment);
      if (b.equal(Boolean(true))){
        return this.consequence.evaluate(enviroment);
      }
      else if (b.equal(Boolean(false))){
        return this.alternative.evaluate(enviroment);
      }
    });

  var While = Struct('While', ['condition', 'body'])
    .method('to_s', function(){
      return this.render('while (<%condition%>) { <%body%> }');
    })
    .method('to_js', function(){
      return Utils.render(
        '(function(e){ while (<%condition%>(e)) {var e = <%body%>(e)};return e; })',
        { condition: this.condition.to_js(),
          body: this.body.to_js()}
      );
    })
    .method('inspect', '*', function(){
      return this.render('<< <%this%> >>');
    })
    .method('is_reducible', function(){return true})
    .method('reduce', function(enviroment){
      return [If(this.condition, Sequence(this.body, this), DoNothing()), enviroment];
    })
    .method('evaluate', function(enviroment){
      var b = this.condition.evaluate(enviroment);
      if (b.equal(Boolean(true))){
        return this.evaluate(this.body.evaluate(enviroment));
      }
      else if (b.equal(Boolean(false))){
        return enviroment;
      }
    });


  var Sequence = Struct('Sequence', ['first', 'second'])
    .method('to_s', function(){
      return this.render('<%first%>; <%second%>');
    })
    .method('to_js', function(){
      return Utils.render(
        '(function(e){ return <%second%>(<%first%>(e)) })',
        { first: this.first.to_js(),
          second: this.second.to_js()}
      );
    })
    .method('inspect', '*', function(){
      return this.render('<< <%this%> >>');
    })
    .method('is_reducible', function(){return true})
    .method('reduce', function(enviroment){
      if (this.first.equal(DoNothing())){
        return [this.second, enviroment];
      }
      else {
        var tmp = this.first.reduce(enviroment);
        var reduced_first = tmp[0];
        var reduced_environment = tmp[1];
        return [Sequence(reduced_first, this.second), reduced_environment];
      }
    })
    .method('evaluate', function(enviroment){
      return this.second.evaluate(this.first.evaluate(enviroment));
    });

  var Machine = Struct('Machine', ['statement', 'enviroment'])
    .method('step', function(){
      var tmp = this.statement.reduce(this.enviroment);
      this.statement = tmp[0];
      this.enviroment = tmp[1];
    })
    .method('run', function(){
      while (this.statement.is_reducible()){
        console.log(this.statement, this.enviroment);
        this.step();
      }
      console.log(this.statement, this.enviroment);
    });

  return {
    Number: Number,
    Boolean: Boolean,
    Variable: Variable,
    Add: Add,
    Multipy: Multipy,
    LessThan: LessThan,
    DoNothing: DoNothing,
    Assign: Assign,
    If: If,
    While: While,
    Sequence: Sequence,
    Machine: Machine,
    Enviroment: Enviroment
  }
});


})(this, typeof define !== 'undefined' && define);
