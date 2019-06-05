observeEvent(input$top_level, {
  input$top_level
  if (!is.null(values$allData) && input$top_level == "Assay data" && is.null(assay_vals$assay_data)) {
    
    closeAlert(session, "nonnumeric")
    
    extracted_data <- withProgress(process_expression(values$allData, session = session), message = "Processing assay data")
    
    assay_vals$warning_state <- extracted_data[["status"]]
    
    assay_vals$orig_data <- extracted_data[["expressionData"]]
    assay_vals$orig_feature <- extracted_data[["featureData"]]
    
    if (is.null(assay_vals$orig_data)) {
      assay_vals$display_default <- data.frame(paste0("No assay data available for ", input$geoID))
      assay_vals$ft_default <- data.frame(paste0("No feature data available for ", input$geoID))
    } else if (is.null(assay_vals$orig_feature)) {
      assay_vals$ft_default <- data.frame(paste0("No feature data available for ", input$geoID))
    }
    else {
      assay_vals$id_col <- colnames(assay_vals$orig_data)[1]
      assay_vals$prev_id <- assay_vals$id_col
      assay_vals$ft_id_col <- colnames(assay_vals$orig_feature)[1]
      assay_vals$ft_prev_id <- assay_vals$ft_id_col
      
      assay_vals$last_data <- assay_vals$orig_data
      assay_vals$assay_data <- assay_vals$orig_data
      
      #assay_vals$orig_feature <- find_intersection(assay_vals$orig_feature, assay_vals$assay_data)
      assay_vals$last_feature <- assay_vals$orig_feature
      assay_vals$feature_data <- assay_vals$orig_feature
      
      assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                       start = 1, 
                                                       forward_distance = 5, 
                                                       previous_view = assay_vals$assay_data)
      assay_vals$feature_display <- advance_columns_view(assay_vals$feature_data, 
                                                        start = 1, 
                                                        forward_distance = 4, 
                                                        previous_view = assay_vals$feature_data)
      #WRITING COMMANDS TO R SCRIPT
      assay_vals$oFile <- saveLines(commentify("extract expression data"), assay_vals$oFile)
      assay_vals$oFile <- saveLines(c("extracted_data <- process_expression(series_data)",
                                             "expressionData <- extracted_data[['expressionData']]",
                                             "featureData <- extracted_data[['featureData']]"), 
                                           assay_vals$oFile)
      
      assay_vals$download_chunk_len <- length(assay_vals$oFile)
    }
    rm(extracted_data)
    shinyjs::toggleState("undoEvalExpr", FALSE)
  }
})

output$data_title <- renderUI({
  if (input$display_assay_or_feature) {
    tagList(
      h4("Feature Data"),
      div(em("Experssion profiling analysis usually generates quantitative data for features of interest. 
             Features of interest may be genes, transcripts, exons, miRNA, or some other genetic entity."),
          a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/info/seq.html", "(Read more)")))
  } else {
    tagList(
      h4("Assay Data"),
      div(em("The abundance measurement of each element derived from each sample."),
          a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/info/overview.html", "(Read more)")))
  }
})

observeEvent(input$resetExpr, {
  if (!is.null(assay_vals$data_to_display)) {
    if (input$display_assay_or_feature) {
      assay_vals$feature_data <- assay_vals$orig_feature
      assay_vals$feature_display <- advance_columns_view(assay_vals$feature_data, 
                                                         start = 1, 
                                                         forward_distance = 4, 
                                                         previous_view = assay_vals$feature_data)
      
    } else {
      assay_vals$disable_btns <- FALSE
      assay_vals$assay_data <- assay_vals$orig_data
      assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                       start = 1, 
                                                       forward_distance = 5, 
                                                       previous_view = assay_vals$assay_data)
      
    }
    assay_vals$oFile <- removeFromScript(assay_vals$oFile, len = assay_vals$download_chunk_len, all = T)
    assay_vals$current_chunk_len <- 0
  }
})

observeEvent(input$expression_next_cols, {
  if (!is.null(assay_vals$data_to_display)) {
    orig <- if (input$display_assay_or_feature) assay_vals$feature_data else assay_vals$assay_data
    assay_vals$data_to_display <- advance_columns_view(orig, 
                                                   start = colnames(assay_vals$data_to_display)[ncol(assay_vals$data_to_display)], 
                                                   forward_distance = 5, 
                                                   previous_view = assay_vals$data_to_display)
  }
})

observeEvent(input$expression_prev_cols, {
  if (!is.null(assay_vals$assay_data)) {
    orig <- if (input$display_assay_or_feature) assay_vals$feature_data else assay_vals$assay_data
    assay_vals$data_to_display <- retract_columns_view(orig, 
                                                   last_column = colnames(assay_vals$data_to_display)[2], 
                                                   backward_distance = 5, 
                                                   previous_view = assay_vals$data_to_display)
  }
})

observe({
  if (!is.null(input$display_assay_or_feature)) {
    if (input$display_assay_or_feature) {
      assay_vals$data_to_display <- assay_vals$feature_display
    } else {
      assay_vals$data_to_display <- assay_vals$assay_display
    }
  }
})

output$exprPreview <- DT::renderDT({
  if (!is.null(assay_vals$data_to_display)) {
    datatable(assay_vals$data_to_display, filter = list(position = "top", clear = FALSE), 
              rownames = FALSE, options = list(dom = "tp"))
  } else {
    datatable(assay_vals$display_default, rownames = FALSE, 
              colnames = "NO DATA", options = list(dom = "tp"))
  }
})

observeEvent(input$expression_evaluate_filters, {
  #TODO: debug filtering when the data is transposed
  
  
  before <- length(assay_vals$oFile)
  assay_vals$oFile <- saveLines(c(commentify("filter data"),
                                  paste0("to_filter <- ", format_string(input$exprPreview_search_columns)),
                                  paste0("names(to_filter) <- ", format_string(colnames(assay_vals$data_to_display)))), 
                                  assay_vals$oFile)
  
  
  
  
  to_filter <- input$exprPreview_search_columns
  names(to_filter) <- colnames(assay_vals$data_to_display)
  if (input$display_assay_or_feature) {
    assay_vals$last_feature <- assay_vals$feature_data
    assay_vals$feature_data <- filterExpressionData(assay_vals$feature_data, to_filter)
    
    #WRITING COMMANDS TO EXPRESSION RSCRIPT
    assay_vals$oFile <-
      saveLines(
        c(
          "featureData <- filterExpressionData(featureData, to_filter)"
        ),
        assay_vals$oFile
      )
  } else {
    assay_vals$last_data <- assay_vals$assay_data
    assay_vals$assay_data <- filterExpressionData(assay_vals$assay_data, to_filter)
    
    #WRITING COMMANDS TO EXPRESSION RSCRIPT
    assay_vals$oFile <-
      saveLines(
        c(
          "expressionData <- filterExpressionData(expressionData, to_filter)"
        ),
        assay_vals$oFile
      )
    
    assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                     start = 1, 
                                                     forward_distance = 5, 
                                                     previous_view = assay_vals$assay_data)
  }
  
  assay_vals$current_chunk_len <- length(assay_vals$oFile) - before
  
  shinyjs::toggleState("undoEvalExpr", TRUE)
})
