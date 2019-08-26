observeEvent(input$resetExpr, {
  if (data_loaded("assay")) {
    reset_datatype("assay")
    
    set_x_equalto_y("disable_btns", FALSE, "assay")
    set_x_equalto_y("id_col", colnames(get_data_member("assay", dataname("assay")))[1], "assay")
    set_x_equalto_y("prev_id", get_data_member("assay", "id_col"), "assay")
    set_x_equalto_y("id_col", colnames(get_data_member("feature", dataname("feature")))[1], "feature")
    set_x_equalto_y("prev_id", get_data_member("feature", "id_col"), "feature")
  }
})

col_navigation_set_server("assay")

output$assay_vals_viewing_subset <- renderUI({
  if (get_data_member("assay", "use_viewing_subset")) {
    col_navigation_set("assay")
  }
})

table_for_col_navigation_server("assay", show_filters = TRUE)

#output$evaluate_filters_button <- renderUI({
#  if (!is.null(input$assay_display_search_columns) && !all(input$assay_display_search_columns == "")) {
#    div(
#      primary_button("expression_evaluate_filters", 
#                     label = div(icon("filter"),
#                                 "Apply filters"),
#                     width = '200px', class = "indent"),
#      help_link("assay", "evaluate_filters_help"))
#  }
#})

#observeEvent(input$expression_evaluate_filters, {
#  #TODO: debug filtering when the data is transposed
#  
#  
#  to_filter <- input$assay_display_search_columns
#  names(to_filter) <- colnames(assay_vals[[dataname("assay")]])[assay_vals$viewing_subset[1]:assay_vals$viewing_subset[2]]
#  status <- eval_function("assay", "filterExpressiondata", list(to_filter), "filter data")
#  if (status != SUCCESS) {
#    showModal(
#      error_modal("Error in filter assay data", "Filters not saved.", status)
#    )
#  }
#  
#  shinyjs::toggleState("undoEvalExpr", TRUE)
#})
