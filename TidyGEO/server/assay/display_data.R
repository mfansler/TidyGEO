observeEvent(input$top_level, {
  input$top_level
  if (!is.null(values$allData) && input$top_level == "Assay data" && is.null(assay_vals$assay_data)) {
    extracted_data <- withProgress(process_expression(values$allData, session))
    
    values$expression_warning_state <- extracted_data[["status"]]
    
    assay_vals$orig_data <- extracted_data[["expressionData"]]
    values$last_expr <- assay_vals$orig_data
    assay_vals$assay_data <- assay_vals$orig_data
    if (is.null(assay_vals$assay_data)) {
      assay_vals$display_default <- data.frame(paste0("No assay data available for ", input$geoID))
      values$default_ft_data <- data.frame(paste0("No feature data available for ", input$geoID))
    } else {
      values$expr_to_display <- advance_columns_view(assay_vals$assay_data, 
                                                     start = 1, 
                                                     forward_distance = 5, 
                                                     previous_view = assay_vals$assay_data)
      values$expression_id_col <- "ID"
      values$expression_prev_id <- values$expression_id_col
      values$orig_feature <- find_intersection(extracted_data[["featureData"]], values$expr_to_display)
      values$last_feature <- values$orig_feature
      values$feature_data <- values$orig_feature
      values$feature_to_display <- advance_columns_view(values$feature_data, 
                                                        start = 1, 
                                                        forward_distance = 4, 
                                                        previous_view = values$feature_data)
      values$feature_id_col <- "ID"
      values$feature_prev_id <- values$feature_id_col
      #WRITING COMMANDS TO R SCRIPT
      values$expression_oFile <- saveLines(commentify("extract expression data"), values$expression_oFile)
      values$expression_oFile <- saveLines(c("extracted_data <- process_expression(series_data)",
                                             "expressionData <- extracted_data[['expressionData']]",
                                             "featureData <- extracted_data[['featureData']]"), 
                                           values$expression_oFile)
      
      values$expression_downloadChunkLen <- length(values$expression_oFile)
    }
    rm(extracted_data)
  }
})
observeEvent(input$expression_next_cols, {
  if (!is.null(assay_vals$assay_data)) {
    values$expr_to_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = colnames(values$expr_to_display)[ncol(values$expr_to_display)], 
                                                   forward_distance = 5, 
                                                   previous_view = values$expr_to_display)
  }
})

observeEvent(input$expression_prev_cols, {
  if (!is.null(assay_vals$assay_data)) {
    values$expr_to_display <- retract_columns_view(assay_vals$assay_data, 
                                                   last_column = colnames(values$expr_to_display)[2], 
                                                   backward_distance = 5, 
                                                   previous_view = values$expr_to_display)
  }
})

output$exprPreview <- DT::renderDT({
  if (!is.null(values$expr_to_display)) {
    datatable(values$expr_to_display, filter = "top", rownames = FALSE, options = list(dom = "tp"))
  } else {
    datatable(assay_vals$display_default, rownames = FALSE, 
              colnames = "NO DATA", options = list(dom = "tp"))
  }
})