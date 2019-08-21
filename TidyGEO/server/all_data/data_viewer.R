#data_to_view_is_null <- reactive({
#  eval(parse(text = paste0("is.null(", input$data_to_view, "_vals$", input$data_to_view, "_data)")))
#})

col_navigation_set_server("all", "data_viewer")

output$all_vals_viewing_subset <- renderUI({
  if (get_data_member(input$data_to_view, "use_viewing_subset")) {
    col_navigation_set(input$data_to_view, "data_viewer")
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
output$cols_visible_clinical_data_viewer <- renderText({
  eval(expr(!!text_expr), list(datatype = "clinical"))
})
output$cols_visible_assay_data_viewer <- renderText({
  eval(expr(!!text_expr), list(datatype = "assay"))
})
output$cols_visible_feature_data_viewer <- renderText({
  eval(expr(!!text_expr), list(datatype = "feature"))
})

output$view_data <- renderUI({
  if (!is.null(input$data_to_view)) {
    # This acts screwy if you try to use one DTOutput for all the datatypes. Don't know why.
    # Best to keep them all separate for now.
    DTOutput(paste0(input$data_to_view, "_data_viewer"))
  }
})

viewer_text_expr <- rlang::expr(
  if (!is.null(get_data_member(datatype, dataname(datatype)))) {
    datatable(do.call(view(datatype), list()), rownames = show_rownames, 
              options = c(BASIC_TABLE_OPTIONS, pageLength = get_data_member(datatype, "user_pagelen")))
  }
  else {
    empty_table(get_data_member(datatype, "display_default"))
  }
)

output$clinical_data_viewer <- renderDT({
  eval(expr(!!viewer_text_expr), list(datatype = "clinical", show_rownames = TRUE))
})

output$assay_data_viewer <- renderDT({
  eval(expr(!!viewer_text_expr), list(datatype = "assay", show_rownames = FALSE))
})

output$feature_data_viewer <- renderDT({
  eval(expr(!!viewer_text_expr), list(datatype = "feature", show_rownames = FALSE))
})

output$all_data_viewer <- renderDT({
  eval(expr(!!viewer_text_expr), list(datatype = "all", show_rownames = TRUE))
})