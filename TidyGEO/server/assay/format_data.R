output$featureData <- DT::renderDT({
  if (!is.null(assay_vals$feature_display)) {
    datatable(assay_vals$feature_display, filter = "top", rownames = FALSE, options = list(dom = "tp", 
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
    datatable(assay_vals$ft_default, rownames = FALSE, 
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
  if (!is.null(assay_vals$feature_data)) {
    assay_vals$feature_display <- advance_columns_view(assay_vals$feature_data, 
                                                      start = colnames(assay_vals$feature_display)[ncol(assay_vals$feature_display)], 
                                                      forward_distance = 4, 
                                                      previous_view = assay_vals$feature_display)
  }
})

observeEvent(input$feature_prev_cols, {
  if (!is.null(assay_vals$feature_data)) {
    assay_vals$feature_display <- retract_columns_view(assay_vals$feature_data, 
                                                      last_column = colnames(assay_vals$feature_display)[2], 
                                                      backward_distance = 4, 
                                                      previous_view = assay_vals$feature_display)
  }
})

observeEvent(input$expression_evaluate_id, {
  
  removeModal()
  
  if (!is.null(assay_vals$assay_data) && assay_vals$ft_id_col != input$colForExprLabels) {
    assay_vals$last_data <- assay_vals$assay_data
    
    feature_data <- assay_vals$feature_data
    
    assay_vals$oFile <- saveLines(c(commentify("replace ID column"),
                                           "featureData2 <- featureData"), assay_vals$oFile)
    
    if (assay_vals$ft_id_col != "ID") {
      
      assay_vals$oFile <- saveLines(
        c(paste0("colnames(featureData2)[which(colnames(featureData2) == 'ID')] <- ", 
                 format_string(colnames(assay_vals$orig_feature[which(colnames(feature_data) == "ID")]))),
          paste0("colnames(featureData2)[which(colnames(featureData2) == ", 
                 format_string(assay_vals$ft_id_col), ")] <- 'ID'")), 
        assay_vals$oFile)
      
      colnames(feature_data)[which(colnames(feature_data) == "ID")] <- 
        colnames(assay_vals$orig_feature[which(colnames(feature_data) == "ID")])
      colnames(feature_data)[which(colnames(feature_data) == assay_vals$ft_id_col)] <- "ID"
      
      if (length(which(colnames(feature_data) == "ID")) > 1) {
        assay_vals$oFile <- saveLines("featureData2 <- featureData2[,-1]", assay_vals$oFile)
        
        #You probably shouldn't be doing this every time
        feature_data <- feature_data[,-1]
      }
    }
    
    assay_vals$assay_data <- withProgress(message = "Replacing the ID column", 
                                     replaceID(assay_vals$assay_data, feature_data, input$colForExprLabels, input$howToSummarize, input$feature_dropNA))
    assay_vals$ft_prev_id <- assay_vals$ft_id_col
    assay_vals$ft_id_col <- input$colForExprLabels
    
    before <- length(assay_vals$oFile)
    
    #WRITING COMMANDS TO EXPRESSION RSCRIPT
    assay_vals$oFile <- saveLines(c(paste0("expressionData <- replaceID(expressionData, featureData2, ", 
                                                  format_string(input$colForExprLabels), ", ",
                                                  format_string(input$howToSummarize), ", ", 
                                                  format_string(input$feature_dropNA), ")")), 
                                         assay_vals$oFile)
    
    assay_vals$assay_display <- advance_columns_view(assay_vals$assay_data, 
                                                   start = 1, 
                                                   forward_distance = 5, 
                                                   assay_vals$assay_data)
    
    after <- length(assay_vals$oFile)
    
    assay_vals$current_chunk_len <- after - before
  }
  
  #updateSelectInput(session, "howToSummarize", selected = "keep all")
  
  #shinyjs::disable("expression_replace_id")
  
})

output$exprLabels <- renderUI({
  selectInput("colForExprLabels", label = div("Please select a column to replace the expression IDs", 
                                              help_button("To keep the same ID column, please choose ID.")), 
              choices = colnames(assay_vals$feature_data)[which(!colnames(assay_vals$feature_data) == "ID")]
  )
})

output$summarizeOptions <- renderUI({
  if (!is.null(input$colForExprLabels) && input$colForExprLabels != "") {
    new_expression_labels <- if (input$feature_dropNA) 
      assay_vals$feature_data[!is.na(input$colForExprLabels), input$colForExprLabels] else assay_vals$feature_data[, input$colForExprLabels]
    can_summarize <- !is_all_unique(new_expression_labels)
    if (can_summarize) {
      choices <- if (assay_vals$warning_state) c("keep all", "mean", "median", "max", "min") else c("keep all")
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
  shinyjs::toggleState("expression_replace_id", condition = !assay_vals$disable_btns)
})

observe({
  disable_transpose <- !assay_vals$disable_btns & length(unique(assay_vals$assay_data$ID)) == nrow(assay_vals$assay_data)
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