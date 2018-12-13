$(document).ready(function() {
  $(document).on('click', '.clinical_plot', function () {
    Shiny.onInputChange('last_btn_clinical',this.id);
  });
  $(document).on('click', '.expression_plot', function () {
    Shiny.onInputChange('last_btn_expression',this.id);
  });
  Shiny.addCustomMessageHandler('resetValue', function(variableName) {
    Shiny.onInputChange(variableName, null);
  });
  $(document).on('click', '.clickimg', function() {
    Shiny.onInputChange('clickimg', $(this).data('value'));
  });
});