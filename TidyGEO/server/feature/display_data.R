observeEvent(input$reset_feature, {
  if (!is.null(feature_vals$data_to_display)) {
    feature_vals$feature_data <- feature_vals$orig_feature
    feature_vals$oFile <- removeFromScript(feature_vals$oFile, len = feature_vals$download_chunk_len, all = T)
    feature_vals$current_chunk_len <- 0
  }
})

output$feature_preview <- DT::renderDT({
  if (!is.null(feature_vals$feature_data)) {
    datatable(feature_vals$feature_data, filter = list(position = "top", clear = FALSE), 
              rownames = FALSE, options = list(dom = "tp"))
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
  
  
  before <- length(feature_vals$oFile)
  feature_vals$oFile <- saveLines(c(commentify("filter data"),
                                  paste0("to_filter <- ", format_string(input$exprPreview_search_columns)),
                                  paste0("names(to_filter) <- ", format_string(colnames(feature_vals$data_to_display)))), 
                                feature_vals$oFile)
  
  
  
  
  to_filter <- input$feature_preview_search_columns
  names(to_filter) <- colnames(feature_vals$data_to_display)
  feature_vals$last_feature <- feature_vals$feature_data
  feature_vals$feature_data <- filterExpressionData(feature_vals$feature_data, to_filter)
  
  #WRITING COMMANDS TO EXPRESSION RSCRIPT
  feature_vals$oFile <-
    saveLines(
      c(
        "featureData <- filterExpressionData(featureData, to_filter)"
      ),
      feature_vals$oFile
    )
  
  feature_vals$current_chunk_len <- length(feature_vals$oFile) - before
})
