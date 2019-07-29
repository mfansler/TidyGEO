output$all_data_workbench <- renderUI({
  if (input$all_data_options == "1") {
    tagList(
      h4("Matching up datasets"),
      p("Here is a preview of the first ten rows of each column you have selected to match."),
      br(),
      fluidRow(
        column(4,
               wellPanel(DTOutput("col_to_match1_preview")),
               uiOutput("col_to_match1_results")
        ),
        column(1, offset = 1,
               br(),
               br(),
               br(),
               br(),
               icon("times", class = "center_align")
        ),
        column(4,
               wellPanel(DTOutput("col_to_match2_preview")),
               uiOutput("col_to_match2_results")
        )
      )
    )
  } else if (input$all_data_options == "2") {
    tagList(
      h4("Joining datasets"),
      p("Here is a preview of the first ten rows of each column you have selected to join."),
      br(),
      fluidRow(
        column(4,
               wellPanel(DTOutput("col_to_match1_preview")),
               uiOutput("col_to_match1_results")
        ),
        column(1, offset = 1,
               br(),
               br(),
               br(),
               br(),
               icon("times", class = "center_align")
        ),
        column(4,
               wellPanel(DTOutput("col_to_match2_preview")),
               uiOutput("col_to_match2_results")
        )
      )
      
    )
    # a preview of the first dataset selected, with the join column highlighted
    # a preview of the second dataset selected, with the join column highlighted
    # a preview of the third dataset selected, with the join column highlighted
    # all separated by something that indicates the join and the direction of the join
    # a preview of the results of the join (maybe include stats such as how many rows before and after,
    # how many columns before and after)
  } else {
    DTOutput("workbench_all_data_view")
  }
})

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


output$workbench_all_data_view <- renderDT({
  
})