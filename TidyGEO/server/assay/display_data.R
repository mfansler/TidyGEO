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
    
    assay_vals$disable_btns <- FALSE
    assay_vals$assay_data <- assay_vals$orig_data
    assay_vals$id_col <- colnames(assay_vals$assay_data)[1]
    assay_vals$prev_id <- assay_vals$id_col
    feature_vals$id_col <- colnames(feature_vals$feature_data)[1]
    feature_vals$prev_id <- feature_vals$id_col
    
    reset_script("assay")
    #assay_vals$oFile <- removeFromScript(assay_vals$oFile, len = assay_vals$download_chunk_len, all = T)
    #assay_vals$current_chunk_len <- 0
  }
})

move_by <- reactive({
  floor(ncol(assay_vals$assay_data) / 5)
})

observeEvent(input$expression_next_cols, {
  if (!is.null(assay_vals$assay_data) && ncol(assay_vals$assay_data) > 6) {
    start <- min(ncol(assay_vals$assay_data), assay_vals$viewing_subset[1] + move_by())
    end <- min(ncol(assay_vals$assay_data), start + move_by())
    assay_vals$viewing_subset <- c(start, end)
  }
})

observeEvent(input$expression_prev_cols, {
  if (!is.null(assay_vals$assay_data) && ncol(assay_vals$assay_data) > 6) {
    end <- max(2, assay_vals$viewing_subset[2] - move_by())
    start <- max(2, end - move_by())
    assay_vals$viewing_subset <- c(start, end)
  }
})

observe({
  assay_vals$viewing_subset <- c(2, min(6, ncol(assay_vals$assay_data)))
}, priority = 1)

assay_in_view <- reactive({
  if (!is.null(assay_vals$assay_data)) {
    assay_vals$assay_data[c(1, assay_vals$viewing_subset[1]:assay_vals$viewing_subset[2])]
  }
})

output$exprPreview <- DT::renderDT({
  if (!is.null(assay_vals$assay_data)) {
    datatable(assay_in_view(), filter = list(position = "top", clear = FALSE), 
              rownames = FALSE, options = list(dom = "tp"))
  } else {
    datatable(assay_vals$display_default, rownames = FALSE, 
              colnames = "NO DATA", options = list(dom = "tp"))
  }
})

observeEvent(input$expression_evaluate_filters, {
  #TODO: debug filtering when the data is transposed
  
  
  #before <- length(assay_vals$oFile)
  #assay_vals$oFile <- saveLines(c(commentify("filter data"),
  #                                paste0("to_filter <- ", format_string(input$exprPreview_search_columns)),
  #                                paste0("names(to_filter) <- ", 
  #                                       format_string(colnames(assay_vals$assay_data)[assay_vals$viewing_subset[1]:assay_vals$viewing_subset[2]]))), 
  #                                assay_vals$oFile)
  
  set_undo_point("assay")
  save_lines(c(commentify("filter data"),
               paste0("to_filter <- ", format_string(input$exprPreview_search_columns)),
               paste0("names(to_filter) <- ", 
               format_string(colnames(assay_vals$assay_data)[assay_vals$viewing_subset[1]:assay_vals$viewing_subset[2]]))), 
             "assay", "body")
  
  
  
  
  to_filter <- input$exprPreview_search_columns
  names(to_filter) <- colnames(assay_vals$assay_data)[assay_vals$viewing_subset[1]:assay_vals$viewing_subset[2]]
  
  assay_vals$last_data <- assay_vals$assay_data
  assay_vals$assay_data <- filterExpressionData(assay_vals$assay_data, to_filter)
  
  #WRITING COMMANDS TO EXPRESSION RSCRIPT
  #assay_vals$oFile <-
  #  saveLines(
  #    c(
  #      "expressionData <- filterExpressionData(expressionData, to_filter)"
  #    ),
  #    assay_vals$oFile
  #  )
  #assay_vals$current_chunk_len <- length(assay_vals$oFile) - before
  
  add_function("filterExpressionData", "assay")
  save_lines(c("expressionData <- filterExpressionData(expressionData, to_filter)"),
      "assay", "body")
  
  shinyjs::toggleState("undoEvalExpr", TRUE)
})
