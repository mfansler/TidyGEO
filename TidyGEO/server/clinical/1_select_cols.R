observe({
  updateCheckboxGroupInput(
    session, 'varsToKeep', choices = colnames(clinical_vals[[dataname("clinical")]]),
    selected = if (input$select_all_columns) colnames(clinical_vals[[dataname("clinical")]])
  )
})

output$display_vars_to_keep <- renderUI({
  #colNames <- colnames(clinical_vals[[dataname("clinical")]][-which(colnames(clinical_vals[[dataname("clinical")]]) == "evalSame")])
  colNames <- colnames(clinical_vals[[dataname("clinical")]])
  checkboxGroupInput(inputId = "varsToKeep", label = NULL, 
                     choices = colNames, selected = colNames)
})

observeEvent(input$clinical_evaluate_filters, ({
  if (!is.null(clinical_vals[[dataname("clinical")]])) {
    
    status <- if (input$filter_option == "preset_filters") {
      eval_function("clinical", "filterUninformativeCols", list(input$download_data_filter), "exclude undesired columns")
    } else {
      eval_function("clinical", "filterCols", list(input$varsToKeep), "exclude undesired columns")
    }
    if (status != SUCCESS) {
      showModal(
        error_modal("Error in filtering columns", "Columns not filtered.", status)
      )
    }
  }
}))

observeEvent(input$undo_select, {
  undo_last_action("clinical")
})