showModal(
  modalDialog(
    includeMarkdown("help_docs/Regular_Expressions_Documentation.md"),
    hr(),
    h4("Regular Expression Tester"),
    sidebarLayout(
      sidebarPanel(
        textInput("regex_to_test", "You can test a regular expression by entering it into the box below:"),
        uiOutput("choose_col_to_test"),
        primary_button("test_regex", "Test")
      ),
      mainPanel(
        HTML('Any matches found by your regular expression will be highlighted here in <span style="background-color: #FFFF00">yellow.</span>'),
        wellPanel(
          DTOutput("col_match_results")
        )
      )
    ),
    footer = modalButton("Close"),
    size = "l"
  )
)

selected_regex_dt <- reactive(
  get_data_member(values$regex_dt, "data")
)

output$choose_col_to_test <- renderUI({
  selectInput("col_to_test", 
              "Please choose a column to use to test your regular expression:", 
              choices = colnames(selected_regex_dt()))
})

output$col_match_results <- renderDT({
  datatable(match_results(), options = list(dom = "t", scrollY = 300, paging = FALSE), rownames = FALSE, escape = FALSE)
})

match_results <- eventReactive(input$test_regex, {
  result <- as.data.frame(str_replace_all(selected_regex_dt()[,input$col_to_test], input$regex_to_test, function(x) {
    paste0('<span style="background-color: #FFFF00">', x, '</span>')
  }))
  colnames(result) <- c(input$col_to_test)
  result
})