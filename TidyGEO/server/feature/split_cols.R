output$feature_choose_cols_to_divide <- renderUI({
  checkboxGroupInput(inputId = "feature_colsToDivide", label = NULL, current_colnames_feature())
})

observe({
  updateCheckboxGroupInput(
    session, 'feature_colsToDivide', choices = current_colnames_feature(),
    selected = if (input$feature_select_all_divide) current_colnames_feature()
  )
})


observeEvent(input$feature_split_cols, ({
  if (data_loaded("feature")) {
    withProgress({
      status <- eval_function("feature", "splitCombinedVars", 
                    list(input$feature_colsToDivide, input$feature_divide_delimiter, input$feature_split_cols_w_regex), 
                    "extract values from columns with delimiter"
                    )
      }, 
      message = "Splitting combined variables"
      )
    if (status != SUCCESS) {
      showModal(
        errorModal("Error in split columns", "Columns not split.", status)
      )
    }
  }
  
}))

observeEvent(input$feature_undo_split_cols, {
  undo_last_action("feature")
})

navigation_set_server("3", "4", "5", "feature_side_panel", "feature_side_panel")
