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
    
    before <- length(assay_vals$oFile)
    
    assay_vals$assay_data <- withProgress(message = "Transposing the data", 
                                     quickTranspose(assay_vals$assay_data))
    
    assay_vals$prev_id <- assay_vals$id_col
    assay_vals$id_col <- "colnames"
    
    #WRITING COMMANDS TO EXPRESSION RSCRIPT
    assay_vals$oFile <- saveLines(c(commentify("transpose data"), 
                                           "expressionData <- quickTranspose(expressionData)"), 
                                         assay_vals$oFile)
    
    assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = 1, 
                                                   forward_distance = 5, 
                                                   previous_view = assay_vals$assay_data)
    
    after <- length(assay_vals$oFile)
    
    assay_vals$current_chunk_len <- after - before
    assay_vals$disable_btns <- TRUE
  }
})



observeEvent(input$expression_evaluate_filters, {
  #TODO: debug filtering when the data is transposed
  assay_vals$last_data <- assay_vals$assay_data
  assay_vals$last_feature <- assay_vals$feature_data
  
  to_filter <- input$exprPreview_search_columns
  names(to_filter) <- colnames(assay_vals$assay_display)
  
  assay_vals$assay_data <- filterExpressionData(assay_vals$assay_data, to_filter)
  assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                 start = 1, 
                                                 forward_distance = 5, 
                                                 previous_view = assay_vals$assay_data)
  assay_vals$feature_data <- find_intersection(assay_vals$feature_data, assay_vals$assay_data, assay_vals$ft_id_col, assay_vals$id_col)
  assay_vals$feature_display <- advance_columns_view(assay_vals$feature_data, 
                                                    start = 1, 
                                                    forward_distance = 4, 
                                                    previous_view = assay_vals$feature_data)
  
  #WRITING COMMANDS TO EXPRESSION RSCRIPT
  before <- length(assay_vals$oFile)
  assay_vals$oFile <- saveLines(c(commentify("filter data"),
                                         paste0("to_filter <- ", format_string(input$exprPreview_search_columns)),
                                         paste0("names(to_filter) <- ", format_string(colnames(assay_vals$assay_display))),
                                         "expressionData <- filterExpressionData(expressionData, to_filter)",
                                         paste0("expressionIdCol <- ", format_string(assay_vals$id_col)),
                                         paste0("featureIdCol <- ", format_string(assay_vals$ft_id_col)),
                                         "featureData <- find_intersection(featureData, expressionData, featureIdCol, expressionIdCol)"), 
                                       assay_vals$oFile)
  assay_vals$current_chunk_len <- length(assay_vals$oFile) - before
})

observeEvent(input$undoEvalExpr, {
  if (!is.null(assay_vals$assay_data)) {
    
    #if replaceID is in the last chunk, then enable the button again
    file_len <- length(assay_vals$oFile)
    if (any(grepl("quickTranspose", assay_vals$oFile[(file_len - (assay_vals$current_chunk_len - 1)):file_len]))) {
      assay_vals$disable_btns <- FALSE
    }
    
    assay_vals$ft_id_col <- assay_vals$ft_prev_id
    assay_vals$feature_data <- assay_vals$last_feature
    assay_vals$feature_display <- advance_columns_view(assay_vals$feature_data, 
                                                      start = 1, 
                                                      forward_distance = 4, 
                                                      previous_view = assay_vals$feature_data)
    assay_vals$id_col <- assay_vals$prev_id
    assay_vals$assay_data <- assay_vals$last_data
    assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = 1, 
                                                   forward_distance = 5, 
                                                   previous_view = assay_vals$assay_data)
    assay_vals$oFile <- removeFromScript(assay_vals$oFile, len = assay_vals$current_chunk_len)
    assay_vals$current_chunk_len <- 0
  }
})

observeEvent(input$resetExpr, {
  if (!is.null(assay_vals$assay_data)) {
    assay_vals$disable_btns <- FALSE
    assay_vals$feature_data <- assay_vals$orig_feature
    assay_vals$feature_display <- advance_columns_view(assay_vals$feature_data, 
                                                      start = 1, 
                                                      forward_distance = 4, 
                                                      previous_view = assay_vals$feature_data)
    assay_vals$assay_data <- assay_vals$orig_data
    assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = 1, 
                                                   forward_distance = 5, 
                                                   previous_view = assay_vals$assay_data)
    assay_vals$oFile <- removeFromScript(assay_vals$oFile, len = assay_vals$download_chunk_len, all = T)
    assay_vals$current_chunk_len <- 0
  }
})