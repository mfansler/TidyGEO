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
    
    set_undo_point("clinical")
    save_lines(commentify("exclude undesired columns"), "clinical", "body")
    #before <- length(clinical_vals$oFile)
    #clinical_vals$oFile <- saveLines(commentify("exclude undesired columns"), clinical_vals$oFile)
    
    if (input$filter_option == "preset_filters") {
      clinical_vals$clinical_data <- filterUninformativeCols(clinical_vals$clinical_data, input$download_data_filter)
      
      #WRITING COMMANDS TO R SCRIPT
      #clinical_vals$oFile <- saveLines(paste0("vars_to_exclude <- ", format_string(input$download_data_filter)), clinical_vals$oFile)
      #clinical_vals$oFile <- saveLines("clinical_data <- filterUninformativeCols(clinical_data, vars_to_exclude)", 
      #                          clinical_vals$oFile)
      
      add_function("filterUninformativeCols", "clinical")
      save_lines(paste0("vars_to_exclude <- ", format_string(input$download_data_filter)), "clinical", "body")
      save_lines("clinical_data <- filterUninformativeCols(clinical_data, vars_to_exclude)", 
                                       "clinical", "body")
    } else {
      clinical_vals$clinical_data <- filterCols(clinical_vals$clinical_data, input$varsToKeep)
      
      #WRITING COMMANDS TO R SCRIPT
      #clinical_vals$oFile <- saveLines(paste0("vars_to_keep <- ", format_string(input$varsToKeep)), clinical_vals$oFile)
      #clinical_vals$oFile <- saveLines(c("clinical_data <- filterCols(clinical_data, vars_to_keep)"), 
      #                          clinical_vals$oFile)
      
      add_function("filterCols", "clinical")
      save_lines(paste0("vars_to_keep <- ", format_string(input$varsToKeep)), "clinical", "body")
      save_lines(c("clinical_data <- filterCols(clinical_data, vars_to_keep)"), 
                                       "clinical", "body")
    }
    
    
    #clinical_vals$current_chunk_len <- length(clinical_vals$oFile) - before
  }
}))

observeEvent(input$undo_select, {
  undo_last_action()
})