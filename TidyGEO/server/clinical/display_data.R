observeEvent(input$reset, {
  reset_datatype("clinical")
})

col_navigation_set_server("clinical")

output$clinical_vals_viewing_subset <- renderUI({
  if (clinical_vals$use_viewing_subset) {
    col_navigation_set("clinical")
  }
})

table_for_col_navigation_server("clinical", show_rownames = TRUE)