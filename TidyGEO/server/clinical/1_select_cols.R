observe({
  updateCheckboxGroupInput(
    session, 'varsToKeep', choices = colnames(clinical_vals$clinical_data),
    selected = if (input$select_all_columns) colnames(clinical_vals$clinical_data)
  )
})

output$display_vars_to_keep <- renderUI({
  #colNames <- colnames(clinical_vals$clinical_data[-which(colnames(clinical_vals$clinical_data) == "evalSame")])
  colNames <- colnames(clinical_vals$clinical_data)
  checkboxGroupInput(inputId = "varsToKeep", label = NULL, 
                     choices = colNames, selected = colNames)
})

observeEvent(input$clinical_evaluate_filters, ({
  if (!is.null(clinical_vals$clinical_data)) {
    clinical_vals$last_data <- clinical_vals$clinical_data
    
    set_undo_point_script("clinical")
    save_lines(commentify("exclude undesired columns"), "clinical", "body")
    #before <- length(clinical_vals$oFile)
    #clinical_vals$oFile <- saveLines(commentify("exclude undesired columns"), clinical_vals$oFile)
    
    if (input$filter_option == "preset_filters") {
      eval_function("clinical", "filterUninformativeCols", list(input$download_data_filter))
    } else {
      eval_function("clinical", "filterCols", list(input$varsToKeep))
    }
    
    
    #clinical_vals$current_chunk_len <- length(clinical_vals$oFile) - before
  }
}))

observeEvent(input$undo_select, {
  undo_last_action()
})