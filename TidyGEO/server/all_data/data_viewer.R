data_viewer_tag <- "data_viewer"

for (dt in ALLOWED_DATATYPES) {
  col_navigation_set_server(dt, data_viewer_tag)  
}

output$all_vals_viewing_subset <- renderUI({
  if (get_data_member(input$data_to_view, "use_viewing_subset")) {
    col_navigation_set(input$data_to_view, data_viewer_tag)
  }
})

output$view_data <- renderUI({
  if (!is.null(input$data_to_view)) {
    # This acts screwy if you try to use one DTOutput for all the datatypes. Don't know why.
    # Best to keep them all separate for now.
    table_for_col_navigation(input$data_to_view, data_viewer_tag)
    #DTOutput(display(input$data_to_view, data_viewer_tag))
  }
})

table_for_col_navigation_server("clinical", data_viewer_tag, show_rownames = TRUE)

table_for_col_navigation_server("assay", data_viewer_tag)

table_for_col_navigation_server("feature", data_viewer_tag)

table_for_col_navigation_server("all", data_viewer_tag, show_rownames = TRUE)