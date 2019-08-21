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
    if (status != SUCCESS) {
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

observeEvent(input$undoEvalExpr, {
  if (!is.null(assay_vals$assay_data)) {
    undo_last_action("assay")
    
    assay_vals$disable_btns <- FALSE
    assay_vals$id_col <- assay_vals$prev_id
    feature_vals$id_col <- feature_vals$prev_id
    shinyjs::disable("undoEvalExpr")
  }
})