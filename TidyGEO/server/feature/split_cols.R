output$feature_choose_cols_to_divide <- renderUI({
  #colNames <- colnames(feature_vals$feature_data[-which(colnames(feature_vals$feature_data) == "evalSame")])
  colNames <- colnames(feature_vals$feature_data)
  checkboxGroupInput(inputId = "colsToDivide", label = NULL, colNames)
})

observe({
  updateCheckboxGroupInput(
    session, 'colsToDivide', choices = colnames(feature_vals$feature_data),
    selected = if (input$feature_select_all_divide) colnames(feature_vals$feature_data)
  )
})


observeEvent(input$feature_split_cols, ({
  if (!is.null(feature_vals$feature_data)) {
    feature_vals$last_feature <- feature_vals$feature_data
    before <- length(feature_vals$oFile)
    feature_vals$oFile <- saveLines(commentify("extract values from columns with delimiter"), feature_vals$oFile)
    feature_vals$feature_data <- withProgress(splitCombinedVars(feature_vals$feature_data,
                                                                  input$feature_colsToDivide,
                                                                  input$feature_divide_delimiter,
                                                                  input$feature_split_cols_w_regex), message = "Splitting combined variables")
    #WRITING COMMANDS TO R SCRIPT
    feature_vals$oFile <- saveLines(paste0("cols_to_divide <- ", format_string(input$feature_colsToDivide)), feature_vals$oFile)
    feature_vals$oFile <- saveLines(c(paste0("divide_delimiter <- ", format_string(input$feature_divide_delimiter)),
                                       paste0("divide_regex <- ", format_string(input$feature_split_cols_w_regex)),
                                       "feature_data <- splitCombinedVars(feature_data, cols_to_divide, divide_delimiter, divide_regex)"), 
                                     feature_vals$oFile)
    feature_vals$current_chunk_len <- length(feature_vals$oFile) - before
  }
  
}))

observeEvent(input$feature_undo_split_cols, {
  undo_last_action_feature()
})