output$choose_cols_to_split_feature <- renderUI({
  colNames <- setdiff(colnames(feature_vals$feature_data), "ID")
  checkboxGroupInput(inputId = "cols_to_split_feature", label = NULL, colNames)
})

observe({
  if (!is.null(input$select_all_split_feature)) {
    updateCheckboxGroupInput(
      session, 'cols_to_split_feature', 
      selected = if (input$select_all_split_feature) setdiff(colnames(feature_vals$feature_data), "ID")
    )
  }
})

observeEvent(input$split_pairs_feature, ({
  if (!is.null(feature_vals$feature_data)) {
    feature_vals$last_feature <- feature_vals$feature_data
    #before <- length(feature_vals$oFile)
    #feature_vals$oFile <- saveLines(commentify("extract key-value pairs from columns with delimiter"), feature_vals$oFile)
    
    set_undo_point_script("feature")
    save_lines(commentify("extract key-value pairs from columns with delimiter"), "feature", "body")
    add_function("extractColNames", "feature")
    save_lines(paste0("cols_to_split <- ", format_string(input$cols_to_split_feature)), "feature", "body")
    save_lines(c(paste0("split_delimiter <- ", format_string(input$split_delimiter_feature)),
                paste0("split_use_regex <- ", format_string(input$split_pairs_w_regex_feature)),
                "feature_data <- extractColNames(clinical_data, split_delimiter, cols_to_split, split_use_regex)"), 
              "feature", "body")
    
    feature_vals$feature_data <- withProgress(extractColNames(feature_vals$feature_data,
                                                                input$split_delimiter_feature,
                                                                input$cols_to_split_feature,
                                                                input$split_pairs_w_regex_feature), message = "Extracting column names")
    #WRITING COMMANDS TO R SCRIPT
    #feature_vals$oFile <- saveLines(paste0("cols_to_split <- ", format_string(input$cols_to_split_feature)), feature_vals$oFile)
    #feature_vals$oFile <- saveLines(c(paste0("split_delimiter <- ", format_string(input$split_delimiter_feature)),
    #                                   paste0("split_use_regex <- ", format_string(input$split_pairs_w_regex_feature)),
    #                                   "feature_data <- extractColNames(clinical_data, split_delimiter, cols_to_split, split_use_regex)"), 
    #                                 feature_vals$oFile)
    
    #feature_vals$current_chunk_len <- length(feature_vals$oFile) - before
  }
  
}))

observeEvent(input$undo_split_pairs, {
  undo_last_action_feature()
})