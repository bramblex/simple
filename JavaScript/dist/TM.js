
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
    .method('is_applies_to', function(configuration){
      return equal(this.state, configuration.state)
      && equal(this.character, configuration.tape.middle)
    })
    .method('follow', function(configuration){
      return TMConfiguration(this.next_state, this.next_tap(configuration));
    })
    .method('next_tap', function(configuration){
      var written_tape = configuration.tape.write(this.write_character);
      switch (this.direction){
        case 'left':
          return written_tape.move_head_left();
        case 'right':
          return written_tape.move_head_right();
      }
    });

  var DTMRulebook = Struct('DTMRulebook', ['rules'])
    .method('next_configuration', function(configuration){
      return this.rule_for(configuration).follow(configuration);
    })
    .method('rule_for', function(configuration){
      return this.rules.reduce(function(last, rule){
        return last || (rule.is_applies_to(configuration) && rule || null )
      }, null);
    })
    .method('is_applies_to', function(configuration){
      return !!this.rule_for(configuration);
    });

  var DTM = Struct('DTM', ['current_configuration', 'accept_states', 'rulebook'])
    .method('is_accepting', function(){
      var _this = this;
      return this.accept_states.reduce(function(last, state){
        return last || equal(_this.current_configuration.state, state)
      }, false);
    })
    .method('step', function(){
      this.current_configuration = this.rulebook.next_configuration(this.current_configuration);
    })
    .method('is_stuck', function(){
      return !this.is_accepting() && !this.rulebook.is_applies_to(this.current_configuration);
    })
    .method('run', function(){
      while (!this.is_accepting() && !this.is_stuck()){
        this.step();
      }
    });

  return {
    Tape: Tape,
    TMConfiguration: TMConfiguration,
    TMRule: TMRule,
    DTMRulebook: DTMRulebook,
    DTM: DTM,
  }
});


})(this, typeof define !== 'undefined' && define);
