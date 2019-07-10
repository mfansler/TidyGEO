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
    assay_vals$last_data <- assay_vals$assay_data
    
    #before <- length(assay_vals$oFile)
    
    assay_vals$assay_data <- withProgress(message = "Transposing the data", 
                                     quickTranspose(assay_vals$assay_data))
    
    assay_vals$prev_id <- assay_vals$id_col
    assay_vals$id_col <- "colnames"
    
    #WRITING COMMANDS TO EXPRESSION RSCRIPT
    #assay_vals$oFile <- saveLines(c(commentify("transpose data"), 
    #                                       "expressionData <- quickTranspose(expressionData)"), 
    #                                     assay_vals$oFile)
    set_undo_point_script("assay")
    add_function("quickTranspose", "assay")
    save_lines(c(commentify("transpose data"), 
                 "expressionData <- quickTranspose(expressionData)"), 
               "assay", "body")
    
    assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = 1, 
                                                   forward_distance = 5, 
                                                   previous_view = assay_vals$assay_data)
    #after <- length(assay_vals$oFile)
    
    assay_vals$current_chunk_len <- after - before
    assay_vals$disable_btns <- TRUE
    shinyjs::toggleState("undoEvalExpr", TRUE)
  }
})

output$evaluate_filters_button <- renderUI({
  if (!is.null(input$exprPreview_search_columns) && !all(input$exprPreview_search_columns == "")) {
    div(
    primary_button("expression_evaluate_filters", 
                   label = div(icon("filter"),
                               "Apply filters"),
                   width = '200px', class = "indent"),
    help_link(id = "evaluate_filters_help"))
  }
})

observeEvent(input$undoEvalExpr, {
  if (!is.null(assay_vals$assay_data)) {
    
    #if replaceID is in the last chunk, then enable the button again
    file_len <- length(assay_vals$oFile)
    if (any(grepl("quickTranspose", assay_vals$oFile[(file_len - (assay_vals$current_chunk_len - 1)):file_len]))) {
      assay_vals$disable_btns <- FALSE
    }
    assay_vals$id_col <- assay_vals$prev_id
    assay_vals$assay_data <- assay_vals$last_data
    assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = 1, 
                                                   forward_distance = 5, 
                                                   previous_view = assay_vals$assay_data)
    assay_vals$oFile <- removeFromScript(assay_vals$oFile, len = assay_vals$current_chunk_len)
    assay_vals$current_chunk_len <- 0
    shinyjs::toggleState("undoEvalExpr", FALSE)
  }
})