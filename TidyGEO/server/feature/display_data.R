observeEvent(input$reset_feature, {
  reset_datatype("feature")
})

col_navigation_set_server("feature")

output$feature_vals_viewing_subset <- renderUI({
  if (feature_vals$use_viewing_subset) {
    col_navigation_set("feature")
  }
})

table_for_col_navigation("feature", show_filters = TRUE)

#output$feature_display <- DT::renderDT({
#  if (!is.null(feature_vals$feature_data)) {
#    datatable(feature_in_view(), filter = list(position = "top", clear = FALSE), 
#              rownames = FALSE, options = c(basic_table_options, pageLength = feature_vals$user_pagelen))
#  } else {
#    empty_table(feature_vals$display_default)
#  }
#})

output$evaluate_filters_button_feature <- renderUI({
  if (!is.null(input$feature_display_search_columns) && !all(input$feature_display_search_columns == "")) {
    div(
      primary_button("feature_evaluate_filters", 
                     label = div(icon("filter"),
                                 "Apply filters"),
                     width = '200px', class = "indent"),
      help_link(id = "evaluate_filters_help_feature"))
  }
})

observeEvent(input$feature_evaluate_filters, {
  #TODO: debug filtering when the data is transposed
  
  
  to_filter <- input$feature_display_search_columns
  names(to_filter) <- colnames(feature_vals$data_to_display)
  status <- eval_function("feature", "filterExpressiondata", list(to_filter), "filter data")
  if (status != SUCCESS) {
    showModal(
      error_modal("Error in filter feature data", "Filters not saved.", status)
    )
  }
})
