output$featureData <- DT::renderDT({
  if (!is.null(values$feature_to_display)) {
    datatable(values$feature_to_display, filter = "top", rownames = FALSE, options = list(dom = "tp", 
                                                                                          pageLength = 5,
                                                                                          columnDefs = list(list(
                                                                                            targets = "_all",
                                                                                            ##Makes it so that the table will only display the first 30 chars.
                                                                                            ##See https://rstudio.github.io/DT/options.html
                                                                                            render = JS(
                                                                                              "function(data, type, row, meta) {",
                                                                                              "return type === 'display' && typeof data === 'string' && data.length > 15 ?",
                                                                                              "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                                                                                              "}")
                                                                                          ))))
  }
  else {
    datatable(values$default_ft_data, rownames = FALSE, 
              colnames = "NO DATA", options = list(dom = "tp"))
  }
}) 

observeEvent(input$expression_replace_id, {
  showModal(
    modalDialog(
      fluidRow(
        column(1, secondary_button(id = "feature_prev_cols", label = div(icon("arrow-left"), "Previous columns"))),
        column(1, offset = 8, secondary_button(id = "feature_next_cols", label = div("Next columns", icon("arrow-right"))))
      ),
      withSpinner(dataTableOutput("featureData"), type = 5),
      uiOutput("exprLabels"),
      uiOutput("summarizeOptions"),
      checkboxInput(inputId = "feature_dropNA", label = div("Drop NA values", 
                                                            help_button(paste("This drops the NA values from the",
                                                                              "column you choose before replacing",
                                                                              "the assay data ID column. This ensures",
                                                                              "that there are no blank entries in",
                                                                              "the ID column.")))),
      
      footer = primary_button(id = "expression_evaluate_id", label = "Replace ID column"), 
      title = "Feature data",
      size = "l",
      easyClose = TRUE
    )
  )
})

observeEvent(input$feature_next_cols, {
  if (!is.null(values$feature_data)) {
    values$feature_to_display <- advance_columns_view(values$feature_data, 
                                                      start = colnames(values$feature_to_display)[ncol(values$feature_to_display)], 
                                                      forward_distance = 4, 
                                                      previous_view = values$feature_to_display)
  }
})

observeEvent(input$feature_prev_cols, {
  if (!is.null(values$feature_data)) {
    values$feature_to_display <- retract_columns_view(values$feature_data, 
                                                      last_column = colnames(values$feature_to_display)[2], 
                                                      backward_distance = 4, 
                                                      previous_view = values$feature_to_display)
  }
})

observeEvent(input$expression_evaluate_id, {
  
  removeModal()
  
  if (!is.null(assay_vals$assay_data) && values$feature_id_col != input$colForExprLabels) {
    values$last_expr <- assay_vals$assay_data
    
    feature_data <- values$feature_data
    
    values$expression_oFile <- saveLines(c(commentify("replace ID column"),
                                           "featureData2 <- featureData"), values$expression_oFile)
    
    if (values$feature_id_col != "ID") {
      
      values$expression_oFile <- saveLines(
        c(paste0("colnames(featureData2)[which(colnames(featureData2) == 'ID')] <- ", 
                 format_string(colnames(values$orig_feature[which(colnames(feature_data) == "ID")]))),
          paste0("colnames(featureData2)[which(colnames(featureData2) == ", 
                 format_string(values$feature_id_col), ")] <- 'ID'")), 
        values$expression_oFile)
      
      colnames(feature_data)[which(colnames(feature_data) == "ID")] <- 
        colnames(values$orig_feature[which(colnames(feature_data) == "ID")])
      colnames(feature_data)[which(colnames(feature_data) == values$feature_id_col)] <- "ID"
      
      if (length(which(colnames(feature_data) == "ID")) > 1) {
        values$expression_oFile <- saveLines("featureData2 <- featureData2[,-1]", values$expression_oFile)
        
        #You probably shouldn't be doing this every time
        feature_data <- feature_data[,-1]
      }
    }
    
    assay_vals$assay_data <- withProgress(message = "Replacing the ID column", 
                                     replaceID(assay_vals$assay_data, feature_data, input$colForExprLabels, input$howToSummarize, input$feature_dropNA))
    values$feature_prev_id <- values$feature_id_col
    values$feature_id_col <- input$colForExprLabels
    
    before <- length(values$expression_oFile)
    
    #WRITING COMMANDS TO EXPRESSION RSCRIPT
    values$expression_oFile <- saveLines(c(paste0("expressionData <- replaceID(expressionData, featureData2, ", 
                                                  format_string(input$colForExprLabels), ", ",
                                                  format_string(input$howToSummarize), ", ", 
                                                  format_string(input$feature_dropNA), ")")), 
                                         values$expression_oFile)
    
    values$expr_to_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = 1, 
                                                   forward_distance = 5, 
                                                   assay_vals$assay_data)
    
    after <- length(values$expression_oFile)
    
    values$expression_currChunkLen <- after - before
  }
  
  #updateSelectInput(session, "howToSummarize", selected = "keep all")
  
  #shinyjs::disable("expression_replace_id")
  
})

output$exprLabels <- renderUI({
  selectInput("colForExprLabels", label = div("Please select a column to replace the expression IDs", 
                                              help_button("To keep the same ID column, please choose ID.")), 
              choices = colnames(values$feature_data)[which(!colnames(values$feature_data) == "ID")]
  )
})

output$summarizeOptions <- renderUI({
  if (!is.null(input$colForExprLabels) && input$colForExprLabels != "") {
    new_expression_labels <- if (input$feature_dropNA) 
      values$feature_data[!is.na(input$colForExprLabels), input$colForExprLabels] else values$feature_data[, input$colForExprLabels]
    can_summarize <- !is_all_unique(new_expression_labels)
    if (can_summarize) {
      choices <- if (values$expression_warning_state) c("keep all", "mean", "median", "max", "min") else c("keep all")
      selectInput("howToSummarize", label = div("It looks like this column contains multiple values for one expression ID.
                                                How would you like to summarize the data?", 
                                                help_button("Groups the data by ID and takes the specified measurement for the group.
                                                            Please note that if you choose 'keep all' you will not be able to transpose
                                                            the data.")), 
                  choices = choices)
    }
    
  }
})

observe({
  shinyjs::toggleState("expression_replace_id", condition = !values$expression_disable_btns)
})

observe({
  disable_transpose <- !values$expression_disable_btns & length(unique(assay_vals$assay_data$ID)) == nrow(assay_vals$assay_data)
  shinyjs::toggleState("expression_transpose", disable_transpose)
})

# other expression options ------------------------------------------------

observeEvent(input$expression_transpose, {
  if (!is.null(assay_vals$assay_data)) {
    values$last_expr <- assay_vals$assay_data
    
    before <- length(values$expression_oFile)
    
    assay_vals$assay_data <- withProgress(message = "Transposing the data", 
                                     quickTranspose(assay_vals$assay_data))
    
    values$expression_prev_id <- values$expression_id_col
    values$expression_id_col <- "colnames"
    
    #WRITING COMMANDS TO EXPRESSION RSCRIPT
    values$expression_oFile <- saveLines(c(commentify("transpose data"), 
                                           "expressionData <- quickTranspose(expressionData)"), 
                                         values$expression_oFile)
    
    values$expr_to_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = 1, 
                                                   forward_distance = 5, 
                                                   previous_view = assay_vals$assay_data)
    
    after <- length(values$expression_oFile)
    
    values$expression_currChunkLen <- after - before
    values$expression_disable_btns <- TRUE
  }
})



observeEvent(input$expression_evaluate_filters, {
  #TODO: debug filtering when the data is transposed
  values$last_expr <- assay_vals$assay_data
  values$last_feature <- values$feature_data
  
  to_filter <- input$exprPreview_search_columns
  names(to_filter) <- colnames(values$expr_to_display)
  
  assay_vals$assay_data <- filterExpressionData(assay_vals$assay_data, to_filter)
  values$expr_to_display <- advance_columns_view(assay_vals$assay_data, 
                                                 start = 1, 
                                                 forward_distance = 5, 
                                                 previous_view = assay_vals$assay_data)
  values$feature_data <- find_intersection(values$feature_data, assay_vals$assay_data, values$feature_id_col, values$expression_id_col)
  values$feature_to_display <- advance_columns_view(values$feature_data, 
                                                    start = 1, 
                                                    forward_distance = 4, 
                                                    previous_view = values$feature_data)
  
  #WRITING COMMANDS TO EXPRESSION RSCRIPT
  before <- length(values$expression_oFile)
  values$expression_oFile <- saveLines(c(commentify("filter data"),
                                         paste0("to_filter <- ", format_string(input$exprPreview_search_columns)),
                                         paste0("names(to_filter) <- ", format_string(colnames(values$expr_to_display))),
                                         "expressionData <- filterExpressionData(expressionData, to_filter)",
                                         paste0("expressionIdCol <- ", format_string(values$expression_id_col)),
                                         paste0("featureIdCol <- ", format_string(values$feature_id_col)),
                                         "featureData <- find_intersection(featureData, expressionData, featureIdCol, expressionIdCol)"), 
                                       values$expression_oFile)
  values$expression_currChunkLen <- length(values$expression_oFile) - before
})

observeEvent(input$undoEvalExpr, {
  if (!is.null(assay_vals$assay_data)) {
    
    #if replaceID is in the last chunk, then enable the button again
    file_len <- length(values$expression_oFile)
    if (any(grepl("quickTranspose", values$expression_oFile[(file_len - (values$expression_currChunkLen - 1)):file_len]))) {
      values$expression_disable_btns <- FALSE
    }
    
    values$feature_id_col <- values$feature_prev_id
    values$feature_data <- values$last_feature
    values$feature_to_display <- advance_columns_view(values$feature_data, 
                                                      start = 1, 
                                                      forward_distance = 4, 
                                                      previous_view = values$feature_data)
    values$expression_id_col <- values$expression_prev_id
    assay_vals$assay_data <- values$last_expr
    values$expr_to_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = 1, 
                                                   forward_distance = 5, 
                                                   previous_view = assay_vals$assay_data)
    values$expression_oFile <- removeFromScript(values$expression_oFile, len = values$expression_currChunkLen)
    values$expression_currChunkLen <- 0
  }
})

observeEvent(input$resetExpr, {
  if (!is.null(assay_vals$assay_data)) {
    values$expression_disable_btns <- FALSE
    values$feature_data <- values$orig_feature
    values$feature_to_display <- advance_columns_view(values$feature_data, 
                                                      start = 1, 
                                                      forward_distance = 4, 
                                                      previous_view = values$feature_data)
    assay_vals$assay_data <- assay_vals$orig_data
    values$expr_to_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = 1, 
                                                   forward_distance = 5, 
                                                   previous_view = assay_vals$assay_data)
    values$expression_oFile <- removeFromScript(values$expression_oFile, len = values$expression_downloadChunkLen, all = T)
    values$expression_currChunkLen <- 0
  }
})