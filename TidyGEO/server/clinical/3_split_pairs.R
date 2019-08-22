output$choose_cols_to_split <- renderUI({
  checkboxGroupInput(inputId = "cols_to_split", label = NULL, clinical_colnames())
})

observe({
  updateCheckboxGroupInput(
    session, 'cols_to_split', choices = clinical_colnames(),
    selected = if (input$select_all_split) clinical_colnames()
  )
})

observeEvent(input$split_pairs, ({
  if (data_loaded("clinical")) {
    withProgress({status <- eval_function("clinical", "extractColNames", 
                  list(input$split_delimiter, input$cols_to_split, input$split_pairs_w_regex),
                  "extract key-value pairs from columns with delimiter")}, message = "Extracting column names")
    if (status != SUCCESS) {
      showModal(
        error_modal("Error in split pairs", "Column names not extracted.", status)
      )
    }
  }
  
}))

observeEvent(input$undo_split_pairs, {
  undo_last_action("clinical")
})

navigation_set_server("2", "3", "4", "clinical_side_panel", "clinical_side_panel")
