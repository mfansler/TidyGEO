current_colnames_feature <- reactive({
  if (data_loaded("feature")) {
    setdiff(colnames(get_data_member("feature", dataname("feature"))), "ID")
  } 
})

output$feature_display_vars_to_keep <- renderUI({
  checkboxGroupInput(inputId = "feature_varsToKeep", label = NULL, 
                     choices = current_colnames_feature(), selected = if (input$feature_select_all_columns) current_colnames_feature())
})

observeEvent(input$feature_evaluate_filters, ({
  if (data_loaded("feature")) {
    
    status <- if (input$feature_filter_option == "preset_filters") {
      eval_function("feature", "filterUninformativeCols", list(input$feature_download_data_filter), "exclude undesired columns")
    } else {
      eval_function("feature", "filterCols", list(c(get_data_member("feature", "id_col"), input$feature_varsToKeep)), "exclude undesired columns")
    }
    if (status != SUCCESS) {
      showModal(
        error_modal("Error in filtering columns", "Columns not filtered.", status)
      )
    }
  }
}))

observeEvent(input$feature_undo_select, {
  undo_last_action("feature")
})

navigation_set_server("1", "2", "3", "feature_side_panel", "feature_side_panel")
