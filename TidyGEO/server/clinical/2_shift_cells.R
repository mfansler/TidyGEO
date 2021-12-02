observeEvent(input$show_broken_cols_example, {
  showModal(
    modalDialog(title = "Data with missing values",
      p('In this example, the values for "Hair" and "Eye" are missing in the second and third rows,
        which has caused "Sex" and "Freq" to shift over into the wrong columns.'),
      DTOutput("broken_cols_example"),
      footer = modalButton("Got it")
    )
  )
})


output$broken_cols_example <- renderDT({
  test_data <- head(as.data.frame(HairEyeColor, stringsAsFactors = FALSE), 5)
  test_data <- mapply(function(x, y) {
    paste(x, y, sep = ": ")
  }, colnames(test_data), test_data)
  test_data[2:3,1:2] <- test_data[2:3, 3:4]
  test_data[2:3, 3:4] <- c(NA, NA)
  datatable(test_data, rownames = FALSE, options = list(dom = "t"))
})

output$choose_cols_to_shift <- renderUI({
  checkboxGroupInput(inputId = "cols_to_shift", label = NULL, clinical_colnames())
})


observe({
  updateCheckboxGroupInput(
    session, 'cols_to_shift', choices = clinical_colnames(),
    selected = if (input$select_all_shift) clinical_colnames()
  )
})


observeEvent(input$shift_pairs, ({
  if (data_loaded("clinical")) {
   withProgress({status <- eval_function("clinical", "shifting_cells", 
                 list(input$shift_pattern, input$cols_to_shift, input$new_col_name),
                  "extract key-value pairs from columns with delimiter")}, message = "Shifting columns")
    if (status != SUCCESS) {
      showModal(
        error_modal("Error in shift columns", "Column not shifted.", status)
      )
    }
  }
  
}))

#observeEvent(input$shift_pairs, shift_cells(input$shift_pattern, input$cols_to_shift, input$new_col_name)

observeEvent(input$undo_shift, {
  undo_last_action("clinical")
})

navigation_set_server("1", "2", "3", "clinical_side_panel", "clinical_side_panel")
