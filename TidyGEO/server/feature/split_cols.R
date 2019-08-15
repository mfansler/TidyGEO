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
    withProgress({
      status <- eval_function("feature", "splitCombinedVars", 
                    list(input$feature_colsToDivide, input$feature_divide_delimiter, input$split_cols_w_regex), 
                    "extract values from columns with delimiter"
                    )
      }, 
      "Splitting combined variables"
      )
    if (status != "completed") {
      showModal(
        errorModal("Error in split columns", "Columns not split.", status)
      )
    }
  }
  
}))

observeEvent(input$feature_undo_split_cols, {
  undo_last_action("feature")
})