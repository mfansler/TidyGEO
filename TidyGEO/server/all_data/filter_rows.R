col_selector_ui <- function(input_id, selector_id) {
  this_input <- get_input(input_id)
  if (!is.null(this_input)) {
    data_to_join <- get_data_member(this_input, dataname(this_input))
    if (data_loaded(this_input)) {
      selectInput(selector_id, 
                  paste("Choose a column from the", this_input, "data to match:"), 
                  choices = c(colnames(data_to_join),
                              "Column names" = "colnames", "Row names" = "rownames")
                  )
    } else {
      HTML(paste0(
        '<p style="color:red">There is no ', this_input, ' data loaded. Please load data in the "',
        toupper(substring(this_input, 1, 1)), substring(this_input, 2, nchar(this_input)), 
        ' data" tab.</p>'))
    }
  }
}

# side panel --------------------------------------------------------------

output$col_to_match1_selector <- renderUI({
  col_selector_ui("data_to_match1", "col_to_match1")
})

output$col_to_match2_selector <- renderUI({
  col_selector_ui("data_to_match2", "col_to_match2")
})

observeEvent(input$match_columns, {
  status <- eval_function(
    input$data_to_match1, "find_intersection", 
    list(data2 = get_data_member_expr(input$data_to_match2, dataname(input$data_to_match2)), 
         id_col1 = input$col_to_match1, id_col2 = input$col_to_match2),
    "matching rows from two columns", 
    to_knit = c(input$data_to_match1, input$data_to_match2)
    )
  if (status != SUCCESS) {
    showModal(
      error_modal("Error in filtering first dataset", "No datasets filtered.", status)
    )
  } else {
    set_x_equalto_y("last_selected_match1", input$data_to_match1, "all")
    status <- eval_function(
      input$data_to_match2, "find_intersection", 
      list(data2 = get_data_member_expr(input$data_to_match1, dataname(input$data_to_match1)), 
           id_col1 = input$col_to_match2, id_col2 = input$col_to_match1),
      "matching rows from two columns",
      to_knit = c(input$data_to_match2, input$data_to_match1)
    )
    if (status != SUCCESS) {
      showModal(
        error_modal("Error in filtering second dataset", "Second dataset not filtered.", status)
      )
    } else {
      set_x_equalto_y("last_selected_match2", input$data_to_match2, "all")
    }
  }
})

observeEvent(input$undo_match, {
  if (!is.null(get_data_member("all", "last_selected_match1"))) {
    undo_last_action(get_data_member("all", "last_selected_match1"))
    set_x_equalto_y("last_selected_match1", NULL, "all")
  }
  if (!is.null(get_data_member("all", "last_selected_match2"))) {
    undo_last_action(get_data_member("all", "last_selected_match2"))
    set_x_equalto_y("last_selected_match2", NULL, "all")
  }
})

observeEvent(get_input(nav("all", "assay")), {
  updateTabsetPanel(session, "top_level", "assay_data")
})
observeEvent(get_input(nav("1", "2", "all")), {
  updateTabsetPanel(session, "all_data_options", selected = "2")
})


# main panel --------------------------------------------------------------

filter_rows_ui <- tagList(
  h4("Matching up datasets"),
  p(paste("Here is a preview of the first ten rows of each column you have selected to match.",
          "To actually merge the datasets, please use the join tool in the next tab.")),
  br(),
  fluidRow(
    column(4,
           DTOutput("col_to_match1_preview"),
           uiOutput("col_to_match1_results")
    ),
    column(1,
           br(),
           br(),
           br(),
           br(),
           icon("times", class = "center_align")
    ),
    column(4,
           DTOutput("col_to_match2_preview"),
           uiOutput("col_to_match2_results")
    )
  )
)

output$col_to_match1_preview <- renderDT({
  datatable(match_vals1(), rownames = FALSE, options = list(dom = "t", scrollY = 200))
})

match_vals1 <- reactive({
  if (!is.null(input$col_to_match1)) {
    if (input$col_to_match1 == "colnames" || input$col_to_match1 == "rownames") {
      names_df <- as.data.frame(
        matrix(
          do.call(input$col_to_match1, list(get_data_member(input$data_to_match1, dataname(input$data_to_match1))))
          )
        )
      colnames(names_df) <- paste0(toupper(substring(input$data_to_match1, 1, 1)), substring(input$data_to_match1, 2), 
                                   if (input$col_to_match1 == "colnames") " column" else " row", " names")
      names_df
    } else {
      get_data_member(input$data_to_match1, dataname(input$data_to_match1))[input$col_to_match1]
    }
  }
})

match_vals2 <- reactive({
  if (!is.null(input$col_to_match2)) {
    if (input$col_to_match2 == "colnames" || input$col_to_match2 == "rownames") {
      names_df <- as.data.frame(
        matrix(
          do.call(input$col_to_match2, list(get_data_member(input$data_to_match2, dataname(input$data_to_match2))))
        )
      )
      colnames(names_df) <- paste0(toupper(substring(input$data_to_match2, 1, 1)), substring(input$data_to_match2, 2), 
                                   if (input$col_to_match2 == "colnames") " column" else " row", " names")
      names_df
    } else {
      get_data_member(input$data_to_match2, dataname(input$data_to_match2))[input$col_to_match2]
    }
  }
})

matched_vals1 <- reactive({
  which(unlist(match_vals1()) %in% unlist(match_vals2()))
})

matched_vals2 <- reactive({
  which(unlist(match_vals2()) %in% unlist(match_vals1()))
})

output$col_to_match1_results <- renderUI({
  HTML(
    paste0(
      "<p><b>Before match: </b>", nrow(match_vals1()), " rows</p>",
      "<p><b>After match: </b>", length(matched_vals1()), " rows</p>"
    )
  )
})

output$col_to_match2_preview <- renderDT({
  datatable(match_vals2(), rownames = FALSE, options = list(dom = "t", scrollY = 200))
})

output$col_to_match2_results <- renderUI({
  HTML(
    paste0(
      "<p><b>Before match: </b>", nrow(match_vals2()), " rows</p>",
      "<p><b>After match: </b>", length(matched_vals2()), " rows</p>"
    )
  )
})
