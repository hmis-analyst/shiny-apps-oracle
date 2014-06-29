// This output binding handles statusOutputBindings
var justgageOutputBinding = new Shiny.OutputBinding();
$.extend(justgageOutputBinding, {
  find: function(scope) {
    return scope.find('.justgage_output');
  },
  renderValue: function(el, data) {
    if (!$(el).data('gauge')) {
      // If we haven't initialized this gauge yet, do it
      $(el).data('gauge', new JustGage({
        id: this.getId(el),
        value: 0,
        min: 0,
        max: 10,
        title: "Degree of difficulty",
        label: "out of 10"
      }));
    }
    $(el).data('gauge').refresh(data);
  }
});
Shiny.outputBindings.register(justgageOutputBinding, 'dashboard.justgageOutputBinding');
