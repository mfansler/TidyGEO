output$display_cols_to_rename <- renderUI({
  #colNames <- colnames(clinical_vals$clinical_data[-which(colnames(clinical_vals$clinical_data) == "evalSame")])
  colNames <- colnames(clinical_vals$clinical_data)
  setNames(colNames, colNames)
  selectInput(inputId = "colsToRename", 
              label = "Which column would you like to rename?", 
              choices = colNames, 
              selected = clinical_vals$last_selected_rename)
})

observeEvent(input$rename, ({
  clinical_vals$last_selected_rename <- input$rename_new_name
  clinical_vals$last_data <- clinical_vals$clinical_data
  clinical_vals$clinical_data <- renameCols(clinical_vals$clinical_data, input$colsToRename, input$rename_new_name)
  
  #WRITING COMMANDS TO R SCRIPT
  before <- length(clinical_vals$oFile)
  clinical_vals$oFile <- saveLines(commentify("rename column"), clinical_vals$oFile)
  clinical_vals$oFile <- saveLines(paste0("clinical_data <- renameCols(clinical_data, ", format_string(input$colsToRename), ", ", format_string(input$rename_new_name), ")"), clinical_vals$oFile)
  clinical_vals$current_chunk_len <- length(clinical_vals$oFile) - before
}))