data_viewer_tag <- "data_viewer"

col_navigation_set_server("all", data_viewer_tag)

output$all_vals_viewing_subset <- renderUI({
  if (get_data_member(input$data_to_view, "use_viewing_subset")) {
    col_navigation_set(input$data_to_view, data_viewer_tag)
  }
})

# I didn't use another col_navigation_set_server call because we only need
# this renderText (no duplicate outputs allowed). The []_in_view() reactives
# already exist in their respective display.R files. If you want the subsets
# in this section to update independently of their original []_in_view()
# reactives, you'll need to do all the work of creating the []_in_view()
# and the viewing subsets by hand.
text_expr <- rlang::expr(paste("Showing", 
                               get_data_member(datatype, "viewing_subset")[1], "to", 
                               get_data_member(datatype, "viewing_subset")[2], "of", 
                               ncol(get_data_member(datatype, dataname(datatype))), 
                               "columns"))
output[[visible("clinical", data_viewer_tag)]] <- renderText({
  eval(expr(!!text_expr), list(datatype = "clinical"))
})
output[[visible("assay", data_viewer_tag)]] <- renderText({
  eval(expr(!!text_expr), list(datatype = "assay"))
})
output[[visible("feature", data_viewer_tag)]] <- renderText({
  eval(expr(!!text_expr), list(datatype = "feature"))
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