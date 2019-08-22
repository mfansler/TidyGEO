clinical_colnames <- reactive({
  colnames(get_data_member("clinical", dataname("clinical")))
})

observe({
  updateCheckboxGroupInput(
    session, 'varsToKeep', choices = clinical_colnames(),
    selected = if (input$select_all_columns) clinical_colnames()
  )
})

output$display_vars_to_keep <- renderUI({
  checkboxGroupInput(inputId = "varsToKeep", label = NULL, 
                     choices = clinical_colnames(), selected = clinical_colnames())
})

observeEvent(input$clinical_evaluate_filters, ({
  if (data_loaded("clinical")) {
    
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

observeEvent(get_input(nav("clinical", "choose")), {
  updateTabItems(session, "top_level", "choose_dataset")
})
observeEvent(get_input(nav("1", "2", "clinical")), {
  updateTabsetPanel(session, "clinical_side_panel", selected = "2")
})
