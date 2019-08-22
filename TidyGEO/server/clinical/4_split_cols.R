output$choose_cols_to_divide <- renderUI({
  checkboxGroupInput(inputId = "colsToDivide", label = NULL, clinical_colnames())
})

observe({
  updateCheckboxGroupInput(
    session, 'colsToDivide', choices = clinical_colnames(),
    selected = if (input$select_all_divide) clinical_colnames()
  )
})


observeEvent(input$split_cols, {
  if (data_loaded("clinical")) {
    withProgress({status <- eval_function("clinical", "splitCombinedVars", 
                               list(input$colsToDivide, input$divide_delimiter, input$split_cols_w_regex),
                               "extract values from columns with delimiter")}, message = "Splitting combined variables")
    if (status != SUCCESS) {
      showModal(
        error_modal("Error in split columns", "Columns not split.", status)
      )
    }
  }
})

observeEvent(input$undo_split_cols, {
  undo_last_action("clinical")
})

navigation_set_server("3", "4", "5", "clinical_side_panel", "clinical_side_panel")
