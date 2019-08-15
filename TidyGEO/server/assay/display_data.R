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
  if (!is.null(assay_vals$assay_data)) {
    reset_datatype("assay")
    
    assay_vals$disable_btns <- FALSE
    assay_vals$id_col <- colnames(assay_vals$assay_data)[1]
    assay_vals$prev_id <- assay_vals$id_col
    feature_vals$id_col <- colnames(feature_vals$feature_data)[1]
    feature_vals$prev_id <- feature_vals$id_col
  }
})

observe({
  assay_vals$use_viewing_subset <- !is.null(ncol(assay_vals$assay_data)) && ncol(assay_vals$assay_data) > 19
  assay_vals$viewing_subset <- c(assay_vals$viewing_min, min(assay_vals$viewing_min + 5, ncol(assay_vals$assay_data)))
}, priority = 1)

assay_in_view <- reactive({
  if (assay_vals$use_viewing_subset) {
    assay_vals$assay_data[c(1, assay_vals$viewing_subset[1]:assay_vals$viewing_subset[2])]
  } else {
    assay_vals$assay_data
  }
})

output$assay_vals_viewing_subset <- renderUI({
  if (assay_vals$use_viewing_subset) {
    col_navigation_set("assay")
  }
})

output$exprPreview <- DT::renderDT({
  if (!is.null(assay_vals$assay_data)) {
    datatable(assay_in_view(), filter = list(position = "top", clear = FALSE), 
              rownames = FALSE, options = list(dom = "tp", scrollX = TRUE))
  } else {
    datatable(assay_vals$display_default, rownames = FALSE, 
              colnames = "NO DATA", options = list(dom = "tp"))
  }
})

observeEvent(input$expression_evaluate_filters, {
  #TODO: debug filtering when the data is transposed
  
  
  to_filter <- input$exprPreview_search_columns
  names(to_filter) <- colnames(assay_vals$assay_data)[assay_vals$viewing_subset[1]:assay_vals$viewing_subset[2]]
  status <- eval_function("assay", "filterExpressiondata", list(to_filter), "filter data")
  if (status != "completed") {
    showModal(
      error_modal("Error in filter assay data", "Filters not saved.", status)
    )
  }
  
  shinyjs::toggleState("undoEvalExpr", TRUE)
})
