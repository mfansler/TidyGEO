#output$data_title <- renderUI({
#  if (input$display_assay_or_feature) {
#    tagList(
#      h4("Feature Data"),
#      div(em("Expression profiling analysis usually generates quantitative data for features of interest. 
#             Features of interest may be genes, transcripts, exons, miRNA, or some other genetic entity."),
#          a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/info/seq.html", "(Read more)")))
#  } else {
#    tagList(
#      h4("Assay Data"),
#      div(em("The abundance measurement of each element derived from each sample."),
#          a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/info/overview.html", "(Read more)")))
#  }
#})

observeEvent(input$resetExpr, {
  if (!is.null(assay_vals[[dataname("assay")]])) {
    reset_datatype("assay")
    
    assay_vals$disable_btns <- FALSE
    assay_vals$id_col <- colnames(assay_vals[[dataname("assay")]])[1]
    assay_vals$prev_id <- assay_vals$id_col
    feature_vals$id_col <- colnames(feature_vals$feature_data)[1]
    feature_vals$prev_id <- feature_vals$id_col
  }
})

col_navigation_set_server("assay")

output$assay_vals_viewing_subset <- renderUI({
  if (assay_vals$use_viewing_subset) {
    col_navigation_set("assay")
  }
})

table_for_col_navigation("assay", show_filters = TRUE)

output$evaluate_filters_button <- renderUI({
  if (!is.null(input$assay_display_search_columns) && !all(input$assay_display_search_columns == "")) {
    div(
      primary_button("expression_evaluate_filters", 
                     label = div(icon("filter"),
                                 "Apply filters"),
                     width = '200px', class = "indent"),
      help_link("assay", "evaluate_filters_help"))
  }
})

observeEvent(input$expression_evaluate_filters, {
  #TODO: debug filtering when the data is transposed
  
  
  to_filter <- input$assay_display_search_columns
  names(to_filter) <- colnames(assay_vals[[dataname("assay")]])[assay_vals$viewing_subset[1]:assay_vals$viewing_subset[2]]
  status <- eval_function("assay", "filterExpressiondata", list(to_filter), "filter data")
  if (status != SUCCESS) {
    showModal(
      error_modal("Error in filter assay data", "Filters not saved.", status)
    )
  }
  
  shinyjs::toggleState("undoEvalExpr", TRUE)
})
