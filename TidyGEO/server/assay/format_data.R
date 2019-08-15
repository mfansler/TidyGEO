source(file.path("server", "assay", "feature_data.R"), local = TRUE)$value

observe({
  shinyjs::toggleState("expression_replace_id", condition = !assay_vals$disable_btns)
})

observe({
  disable_transpose <- !assay_vals$disable_btns & length(unique(assay_vals$assay_data$ID)) == nrow(assay_vals$assay_data)
  #browser()
  shinyjs::toggleState("expression_transpose", disable_transpose)
})

# other expression options ------------------------------------------------

observeEvent(input$expression_transpose, {
  if (!is.null(assay_vals$assay_data)) {
    status <- withProgress(
      eval_function("assay", "quickTranspose", list(), "transpose data"), 
      message = "Transposing the data"
      )
    if (status != "completed") {
      showModal(
        error_modal("Error in transpose", "Data not transposed.", status)
      )
    } else {
      assay_vals$prev_id <- assay_vals$id_col
      assay_vals$id_col <- "colnames"
      assay_vals$disable_btns <- TRUE
      shinyjs::enable("undoEvalExpr")
    }
  }
})

output$evaluate_filters_button <- renderUI({
  if (!is.null(input$exprPreview_search_columns) && !all(input$exprPreview_search_columns == "")) {
    div(
    primary_button("expression_evaluate_filters", 
                   label = div(icon("filter"),
                               "Apply filters"),
                   width = '200px', class = "indent"),
    help_link("assay", "evaluate_filters_help"))
  }
})

observeEvent(input$undoEvalExpr, {
  if (!is.null(assay_vals$assay_data)) {
    
    assay_vals$disable_btns <- FALSE
    assay_vals$id_col <- assay_vals$prev_id
    assay_vals$assay_data <- assay_vals$last_data
    feature_vals$id_col <- feature_vals$prev_id
    undo_script("assay")
    shinyjs::disable("undoEvalExpr")
  }
})