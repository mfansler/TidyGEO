observeEvent(input$top_level, {
  input$top_level
  if (!is.null(values$allData) && input$top_level == "Assay data" && is.null(assay_vals$assay_data)) {
    
    closeAlert(session, "alpha_alert")
    
    extracted_data <- withProgress(process_expression(values$allData, session))
    
    assay_vals$warning_state <- extracted_data[["status"]]
    
    assay_vals$orig_data <- extracted_data[["expressionData"]]
    assay_vals$last_data <- assay_vals$orig_data
    assay_vals$assay_data <- assay_vals$orig_data
    if (is.null(assay_vals$assay_data)) {
      assay_vals$display_default <- data.frame(paste0("No assay data available for ", input$geoID))
      assay_vals$ft_default <- data.frame(paste0("No feature data available for ", input$geoID))
    } else {
      assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                     start = 1, 
                                                     forward_distance = 5, 
                                                     previous_view = assay_vals$assay_data)
      assay_vals$id_col <- "ID"
      assay_vals$prev_id <- assay_vals$id_col
      assay_vals$orig_feature <- find_intersection(extracted_data[["featureData"]], assay_vals$assay_display)
      assay_vals$last_feature <- assay_vals$orig_feature
      assay_vals$feature_data <- assay_vals$orig_feature
      assay_vals$feature_display <- advance_columns_view(assay_vals$feature_data, 
                                                        start = 1, 
                                                        forward_distance = 4, 
                                                        previous_view = assay_vals$feature_data)
      assay_vals$ft_id_col <- "ID"
      assay_vals$ft_prev_id <- assay_vals$ft_id_col
      #WRITING COMMANDS TO R SCRIPT
      assay_vals$oFile <- saveLines(commentify("extract expression data"), assay_vals$oFile)
      assay_vals$oFile <- saveLines(c("extracted_data <- process_expression(series_data)",
                                             "expressionData <- extracted_data[['expressionData']]",
                                             "featureData <- extracted_data[['featureData']]"), 
                                           assay_vals$oFile)
      
      assay_vals$download_chunk_len <- length(assay_vals$oFile)
    }
    rm(extracted_data)
  }
})
observeEvent(input$expression_next_cols, {
  if (!is.null(assay_vals$assay_data)) {
    assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = colnames(assay_vals$assay_display)[ncol(assay_vals$assay_display)], 
                                                   forward_distance = 5, 
                                                   previous_view = assay_vals$assay_display)
  }
})

observeEvent(input$expression_prev_cols, {
  if (!is.null(assay_vals$assay_data)) {
    assay_vals$assay_display <- retract_columns_view(assay_vals$assay_data, 
                                                   last_column = colnames(assay_vals$assay_display)[2], 
                                                   backward_distance = 5, 
                                                   previous_view = assay_vals$assay_display)
  }
})

output$exprPreview <- DT::renderDT({
  if (!is.null(assay_vals$assay_display)) {
    datatable(assay_vals$assay_display, filter = "top", rownames = FALSE, options = list(dom = "tp"))
  } else {
    datatable(assay_vals$display_default, rownames = FALSE, 
              colnames = "NO DATA", options = list(dom = "tp"))
  }
})