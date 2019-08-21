output$display_cols_to_rename <- renderUI({
  #colNames <- colnames(clinical_vals[[dataname("clinical")]][-which(colnames(clinical_vals[[dataname("clinical")]]) == "evalSame")])
  colNames <- colnames(clinical_vals[[dataname("clinical")]])
  setNames(colNames, colNames)
  selectInput(inputId = "colsToRename", 
              label = "Which column would you like to rename?", 
              choices = colNames, 
              selected = clinical_vals$last_selected_rename)
})

observeEvent(input$rename, ({
  if (!input$rename_new_name %in% colnames(clinical_vals[[dataname("clinical")]])) {
    clinical_vals$last_selected_rename <- input$rename_new_name
    status <- eval_function("clinical", "renameCols", list(input$colsToRename, input$rename_new_name), "rename column")
    if (status != SUCCESS) {
      showModal(
        error_modal("Error in rename column", "Column not renamed.", status)
      )
    }
    
  } else {
    showModal(
      modalDialog(
        HTML(
          paste0('<font color="red"> Looks like  "', input$rename_new_name, 
                 '" is already taken. Please choose a different name to avoid duplicates. </font>')
        ),
        title = HTML('<font color="red">Whoops!</font>'), 
        footer = modalButton("OK")
      )
    )
  }
}))

observeEvent(input$undo_rename, {
  undo_last_action("clinical")
})