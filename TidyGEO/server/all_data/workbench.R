output$all_data_workbench <- renderUI({
  if (input$all_data_options == "1") {
    filter_rows_ui
  } else if (input$all_data_options == "2") {
    join_dfs_ui
  } else {
    save_data_ui
  }
})