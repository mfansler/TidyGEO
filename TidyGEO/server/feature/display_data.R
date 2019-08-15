observeEvent(input$reset_feature, {
  reset_datatype("feature")
})

observe({
  feature_vals$use_viewing_subset <- !is.null(ncol(feature_vals$feature_data)) && ncol(feature_vals$feature_data) > 19
  feature_vals$viewing_subset <- c(feature_vals$viewing_min, min(feature_vals$viewing_min + 5, ncol(feature_vals$feature_data)))
}, priority = 1)

feature_in_view <- reactive({
  if (feature_vals$use_viewing_subset) {
    feature_vals$feature_data[c(1, feature_vals$viewing_subset[1]:feature_vals$viewing_subset[2])]
  } else {
    feature_vals$feature_data
  }
})

output$feature_vals_viewing_subset <- renderUI({
  if (feature_vals$use_viewing_subset) {
    col_navigation_set("feature")
  }
})

output$feature_preview <- DT::renderDT({
  if (!is.null(feature_vals$feature_data)) {
    datatable(feature_in_view(), filter = list(position = "top", clear = FALSE), 
              rownames = FALSE, options = list(dom = "tp", scrollX = TRUE))
  } else {
    datatable(feature_vals$ft_default, rownames = FALSE, 
              colnames = "NO DATA", options = list(dom = "tp"))
  }
})

output$evaluate_filters_button_feature <- renderUI({
  if (!is.null(input$feature_preview_search_columns) && !all(input$feature_preview_search_columns == "")) {
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
  
  
  to_filter <- input$feature_preview_search_columns
  names(to_filter) <- colnames(feature_vals$data_to_display)
  status <- eval_function("feature", "filterExpressiondata", list(to_filter), "filter data")
  if (status != "completed") {
    showModal(
      error_modal("Error in filter feature data", "Filters not saved.", status)
    )
  }
})
