col_selector_ui <- function(input_id, selector_id) {
  this_input <- parse(text = paste0("input$", input_id))
  if (!is.null(eval(this_input))) {
    data_to_join <- paste0(eval(this_input), "_vals$", eval(this_input), "_data")
    if (!is.null(eval(parse(text = data_to_join)))) {
      selectInput(selector_id, 
                  paste("Choose a column from the", eval(this_input), "data to join:"), 
                  choices = eval(parse(text = paste0("colnames(", data_to_join, ")"))))
    } else {
      HTML(paste0(
        '<p style="color:red">There is no ', eval(this_input), ' data loaded. Please load data in the "',
        toupper(substring(eval(this_input), 1, 1)), substring(eval(this_input), 2, nchar(eval(this_input))), 
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


# main panel --------------------------------------------------------------

filter_rows_ui <- tagList(
  h4("Matching up datasets"),
  p("Here is a preview of the first ten rows of each column you have selected to match."),
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
  datatable(eval(parse(
    text = paste0(
      input$data_to_match1,
      "_vals$",
      input$data_to_match1,
      '_data["',
      input$col_to_match1,
      '"]'
    )
  )), rownames = FALSE, options = list(dom = "t", scrollY = 200))
})

match_vals1 <- reactive({
  eval(parse(
    text = paste0(
      input$data_to_match1,
      "_vals$",
      input$data_to_match1,
      '_data[, "',
      input$col_to_match1,
      '"]'
    )))
})

match_vals2 <- reactive({
  eval(parse(
    text = paste0(
      input$data_to_match2,
      "_vals$",
      input$data_to_match2,
      '_data[, "',
      input$col_to_match2,
      '"]'
    )))
})

matched_vals1 <- reactive({
  which(match_vals1() %in% match_vals2())
})

matched_vals2 <- reactive({
  which(match_vals2() %in% match_vals1())
})

output$col_to_match1_results <- renderUI({
  HTML(
    paste0(
      "<p><b>Before match: </b>", length(match_vals1()), " rows</p>",
      "<p><b>After match: </b>", length(matched_vals1()), " rows</p>"
    )
  )
})

output$col_to_match2_preview <- renderDT({
  datatable(eval(parse(
    text = paste0(
      input$data_to_match2,
      "_vals$",
      input$data_to_match2,
      '_data["',
      input$col_to_match2,
      '"]'
    )
  )), rownames = FALSE, options = list(dom = "t", scrollY = 200))
})

output$col_to_match2_results <- renderUI({
  HTML(
    paste0(
      "<p><b>Before match: </b>", length(match_vals2()), " rows</p>",
      "<p><b>After match: </b>", length(matched_vals2()), " rows</p>"
    )
  )
})
