output$choose_cols_to_split_feature <- renderUI({
  #colNames <- colnames(assay_vals$clinical_data[-which(colnames(assay_vals$clinical_data) == "evalSame")])
  colNames <- setdiff(colnames(assay_vals$feature_data), "ID")
  checkboxGroupInput(inputId = "cols_to_split_feature", label = NULL, colNames)
})

observe({
  if (!is.null(input$select_all_split_feature)) {
    updateCheckboxGroupInput(
      session, 'cols_to_split_feature', 
      selected = if (input$select_all_split_feature) setdiff(colnames(assay_vals$feature_data), "ID")
    )
  }
})

observeEvent(input$split_pairs_feature, ({
  if (!is.null(assay_vals$feature_data)) {
    assay_vals$last_feature <- assay_vals$feature_data
    before <- length(assay_vals$oFile)
    assay_vals$oFile <- saveLines(commentify("extract key-value pairs from columns with delimiter"), assay_vals$oFile)
    assay_vals$feature_data <- withProgress(extractColNames(assay_vals$feature_data,
                                                                input$split_delimiter_feature,
                                                                input$cols_to_split_feature,
                                                                input$split_pairs_w_regex_feature), message = "Extracting column names")
    #WRITING COMMANDS TO R SCRIPT
    assay_vals$oFile <- saveLines(paste0("cols_to_split <- ", format_string(input$cols_to_split_feature)), assay_vals$oFile)
    assay_vals$oFile <- saveLines(c(paste0("split_delimiter <- ", format_string(input$split_delimiter_feature)),
                                       paste0("split_use_regex <- ", format_string(input$split_pairs_w_regex_feature)),
                                       "clinical_data <- extractColNames(clinical_data, split_delimiter, cols_to_split, split_use_regex)"), 
                                     assay_vals$oFile)
    
    assay_vals$current_chunk_len <- length(assay_vals$oFile) - before
  }
  
}))

observeEvent(input$undo_split_pairs, {
  undo_last_action()
})