output$choose_cols_to_split <- renderUI({
  #colNames <- colnames(clinical_vals$clinical_data[-which(colnames(clinical_vals$clinical_data) == "evalSame")])
  colNames <- colnames(clinical_vals$clinical_data)
  checkboxGroupInput(inputId = "cols_to_split", label = NULL, colNames)
})

observe({
  updateCheckboxGroupInput(
    session, 'cols_to_split', choices = colnames(clinical_vals$clinical_data),
    selected = if (input$select_all_split) colnames(clinical_vals$clinical_data)
  )
})

observeEvent(input$split_pairs, ({
  if (!is.null(clinical_vals$clinical_data)) {
    #clinical_vals$last_data <- clinical_vals$clinical_data
    #before <- length(clinical_vals$oFile)
    #clinical_vals$oFile <- saveLines(commentify("extract key-value pairs from columns with delimiter"), clinical_vals$oFile)
    #clinical_vals$clinical_data <- withProgress(extractColNames(clinical_vals$clinical_data,
    #                                                            input$split_delimiter,
    #                                                            input$cols_to_split,
    #                                                            input$split_pairs_w_regex), message = "Extracting column names")
    withProgress({status <- eval_function("clinical", "extractColNames", 
                  list(input$split_delimiter, input$cols_to_split, input$split_pairs_w_regex),
                  "extract key-value pairs from columns with delimiter")}, message = "Extracting column names")
    if (status != "completed") {
      showModal(
        error_modal("Error in split pairs", "Column names not extracted.", status)
      )
    }
    #WRITING COMMANDS TO R SCRIPT
    #clinical_vals$oFile <- saveLines(paste0("cols_to_split <- ", format_string(input$cols_to_split)), clinical_vals$oFile)
    #clinical_vals$oFile <- saveLines(c(paste0("split_delimiter <- ", format_string(input$split_delimiter)),
    #                                   paste0("split_use_regex <- ", format_string(input$split_pairs_w_regex)),
    #                                   "clinical_data <- extractColNames(clinical_data, split_delimiter, cols_to_split, split_use_regex)"), 
    #                                 clinical_vals$oFile)
    
    #set_undo_point_script("clinical")
    #save_lines(commentify("extract key-value pairs from columns with delimiter"), "clinical", "body")
    #add_function("extractColNames", "clinical")
    #save_lines(paste0("cols_to_split <- ", format_string(input$cols_to_split)), "clinical", "body")
    #save_lines(c(paste0("split_delimiter <- ", format_string(input$split_delimiter)),
    #                                   paste0("split_use_regex <- ", format_string(input$split_pairs_w_regex)),
    #                                   "clinical_data <- extractColNames(clinical_data, split_delimiter, cols_to_split, split_use_regex)"), 
    #                                 "clinical", 'body')
    
    #clinical_vals$current_chunk_len <- length(clinical_vals$oFile) - before
  }
  
}))

observeEvent(input$undo_split_pairs, {
  undo_last_action("clinical")
})