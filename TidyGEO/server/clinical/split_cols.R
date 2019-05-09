output$choose_cols_to_divide <- renderUI({
  #colNames <- colnames(clinical_vals$clinical_data[-which(colnames(clinical_vals$clinical_data) == "evalSame")])
  colNames <- colnames(clinical_vals$clinical_data)
  checkboxGroupInput(inputId = "colsToDivide", label = NULL, colNames)
})

observe({
  updateCheckboxGroupInput(
    session, 'colsToDivide', choices = colnames(clinical_vals$clinical_data),
    selected = if (input$select_all_divide) colnames(clinical_vals$clinical_data)
  )
})


observeEvent(input$split_cols, ({
  if (!is.null(clinical_vals$clinical_data)) {
    clinical_vals$last_data <- clinical_vals$clinical_data
    before <- length(clinical_vals$oFile)
    clinical_vals$oFile <- saveLines(commentify("extract values from columns with delimiter"), clinical_vals$oFile)
    clinical_vals$clinical_data <- withProgress(splitCombinedVars(clinical_vals$clinical_data,
                                                      input$colsToDivide,
                                                      input$divide_delimiter,
                                                      input$split_cols_w_regex), message = "Splitting combined variables")
    #WRITING COMMANDS TO R SCRIPT
    clinical_vals$oFile <- saveLines(paste0("cols_to_divide <- ", format_string(input$colsToDivide)), clinical_vals$oFile)
    clinical_vals$oFile <- saveLines(c(paste0("divide_delimiter <- ", format_string(input$divide_delimiter)),
                                       paste0("divide_regex <- ", format_string(input$split_cols_w_regex)),
                                "clinical_data <- splitCombinedVars(clinical_data, cols_to_divide, divide_delimiter)"), 
                              clinical_vals$oFile)
    clinical_vals$current_chunk_len <- length(clinical_vals$oFile) - before
  }
  
}))

observeEvent(input$undo_split_cols, {
  undo_last_action()
})