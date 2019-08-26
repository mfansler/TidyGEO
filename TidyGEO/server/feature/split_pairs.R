output$choose_cols_to_split_feature <- renderUI({
  checkboxGroupInput(inputId = "cols_to_split_feature", label = NULL, current_colnames_feature())
})

observe({
  if (!is.null(input$select_all_split_feature)) {
    updateCheckboxGroupInput(
      session, 'cols_to_split_feature', 
      selected = if (input$select_all_split_feature) current_colnames_feature()
    )
  }
})

observeEvent(input$split_pairs_feature, {
  if (data_loaded("feature")) {
    withProgress({
      status <- eval_function("feature", "extractColNames", 
                    list(input$split_delimiter_feature, input$cols_to_split_feature, input$split_pairs_w_regex_feature), 
                    "extract key-value pairs from columns with delimiter"
                    )
      }, 
      message = "Extracting column names"
      )
    if (status != SUCCESS) {
      showModal(
        error_modal("Error in extract column names", "Column names not extracted.", status)
      )
    }
  }
  
})

observeEvent(input$undo_split_pairs_feature, {
  undo_last_action("feature")
})

navigation_set_server("2", "3", "4", "feature_side_panel", "feature_side_panel")
