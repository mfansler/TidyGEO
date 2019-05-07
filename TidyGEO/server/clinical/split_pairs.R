output$choose_cols_to_split <- renderUI({
  #colNames <- colnames(clinical_vals$clinical_data[-which(colnames(clinical_vals$clinical_data) == "evalSame")])
  colNames <- colnames(clinical_vals$clinical_data)
  checkboxGroupInput(inputId = "cols_to_split", label = NULL, colNames)
})

observe({
  updateCheckboxGroupInput(
    session, 'cols_to_split', choices = colnames(clinical_vals$clinical_data),
    selected = if (input$select_all_split) colnames(clinical_vals$clinical_data)
  )
})

observeEvent(input$reformat_columns, ({
  if (!is.null(clinical_vals$clinical_data)) {
    clinical_vals$last_data <- clinical_vals$clinical_data
    before <- length(clinical_vals$oFile)
    clinical_vals$oFile <- saveLines(commentify("extract key-value pairs from columns with delimiter"), clinical_vals$oFile)
    clinical_vals$clinical_data <- withProgress(extractColNames(clinical_vals$clinical_data,
                                                                input$split_delimiter,
                                                                input$cols_to_split), message = "Extracting column names")
    #WRITING COMMANDS TO R SCRIPT
    clinical_vals$oFile <- saveLines(paste0("cols_to_split <- ", format_string(input$cols_to_split)), clinical_vals$oFile)
    clinical_vals$oFile <- saveLines(c(paste0("split_delimiter <- ", format_string(input$split_delimiter)), 
                                       "clinical_data <- extractColNames(clinical_data, split_delimiter, cols_to_split)"), 
                                     clinical_vals$oFile)
    
    clinical_vals$current_chunk_len <- length(clinical_vals$oFile) - before
  }
  
}))

observeEvent(input$undo_split_columns, {
  undo_last_action()
})