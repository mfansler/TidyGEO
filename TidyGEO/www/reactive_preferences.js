//https://stackoverflow.com/questions/40168801/r-shiny-last-clicked-button-id

//when I have a bunch of buttons that do the same thing, I can't name the buttons the same thing (duplicate
//IDs not allowed) and I can't have an observeEvent that listens to multiple buttons; instead, I give the
//buttons a class and whenever a button from that class is clicked, the JavaScript here puts that button's
//ID in an input for me to listen to with an observeEvent; after I've used that input, I have to reset it
//with this JavaScript so the next button click can set off the observeEvent again


$(document).ready(function() {
  // subseries select listeners
  $(document).on('click', '.subseries', function () {
    Shiny.onInputChange('subseries_selected', this.id);
  });
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
  $(document).on('click', '.match_help', function () {
    Shiny.onInputChange('match_help_clicked', this.id);
  });
  $(document).on('click', '.join_help', function () {
    Shiny.onInputChange('join_help_clicked', this.id);
  });
});