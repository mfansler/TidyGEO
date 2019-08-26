//https://stackoverflow.com/questions/40168801/r-shiny-last-clicked-button-id

$(document).ready(function() {
  // column navigation listeners
  $(document).on('click', '.prev_cols', function () {
    Shiny.onInputChange('prev_cols_clicked', this.id);
  });
  $(document).on('click', '.next_cols', function () {
    Shiny.onInputChange('next_cols_clicked', this.id);
  });
  // reset an input to NULL
  Shiny.addCustomMessageHandler('resetValue', function(variableName) {
    Shiny.onInputChange(variableName, null);
  });
  // return the value of which image was clicked to expand in a help modal
  $(document).on('click', '.clickimg', function() {
    Shiny.onInputChange('clickimg', $(this).data('value'));
  });
  // help modal listeners
  $(document).on('click', '.shift_help', function () {
    Shiny.onInputChange('shift_help_clicked', this.id);
  });
  $(document).on('click', '.split_help', function () {
    Shiny.onInputChange('split_help_clicked', this.id);
  });
  $(document).on('click', '.divide_help', function () {
    Shiny.onInputChange('divide_help_clicked', this.id);
  });
  $(document).on('click', '.substitute_help', function () {
    Shiny.onInputChange('substitute_help_clicked', this.id);
  });
  $(document).on('click', '.regex_help', function () {
    Shiny.onInputChange('regex_help_clicked', this.id);
  });
  $(document).on('click', '.exclude_help', function () {
    Shiny.onInputChange('exclude_help_clicked', this.id);
  });
  $(document).on('click', '.download_help', function () {
    Shiny.onInputChange('download_help_clicked', this.id);
  });
  $(document).on('click', '.r_help', function () {
    Shiny.onInputChange('r_help_clicked', this.id);
  });
  $(document).on('click', '.replace_id_help', function () {
    Shiny.onInputChange('replace_id_help_clicked', this.id);
  });
  $(document).on('click', '.transpose_help', function () {
    Shiny.onInputChange('transpose_help_clicked', this.id);
  });
  $(document).on('click', '.filter_help', function () {
    Shiny.onInputChange('filter_help_clicked', this.id);
  });
  $(document).on('click', '.evaluate_filters_help', function () {
    Shiny.onInputChange('evaluate_filters_help_clicked', this.id);
  });
  $(document).on('click', '.files_help', function () {
    Shiny.onInputChange('files_help_clicked', this.id);
  });
});