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


observeEvent(input$split_cols, {
  if (!is.null(clinical_vals$clinical_data)) {
    withProgress({status <- eval_function("clinical", "splitCombinedVars", 
                               list(input$colsToDivide, input$divide_delimiter, input$split_cols_w_regex),
                               "extract values from columns with delimiter")}, message = "Splitting combined variables")
    if (status != "completed") {
      showModal(
        error_modal("Error in split columns", "Columns not split.", status)
      )
    }
  }
})

observeEvent(input$undo_split_cols, {
  undo_last_action("clinical")
})