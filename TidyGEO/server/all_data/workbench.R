output$all_data_workbench <- renderUI({
  if (input$all_data_options == "1") {
    filter_rows_ui
  } else if (input$all_data_options == "2") {
    join_dfs_ui
    # a preview of the first dataset selected, with the join column highlighted
    # a preview of the second dataset selected, with the join column highlighted
    # a preview of the third dataset selected, with the join column highlighted
    # all separated by something that indicates the join and the direction of the join
    # a preview of the results of the join (maybe include stats such as how many rows before and after,
    # how many columns before and after)
  } else if (input$all_data_options == "3") {
    save_data_ui
  } else {
    DTOutput("workbench_all_data_view")
  }
})


output$workbench_all_data_view <- renderDT({
  
})