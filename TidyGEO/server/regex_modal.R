showModal(
  modalDialog(
    includeMarkdown("help_docs/Regular_Expressions_Documentation.md"),
    h4("Regular Expression Tester"),
    sidebarLayout(
      sidebarPanel(
        textInput("regex_to_test", "You can test a regular expression by entering it into the box below:"),
        uiOutput("choose_col_to_test"),
        primary_button("test_regex", "Test")
      ),
      mainPanel(
        wellPanel(
          DTOutput("col_match_results")
        )
      )
    ),
    footer = modalButton("Close"),
    size = "l"
  )
)

output$choose_col_to_test <- renderUI({
  selectInput("col_to_test", 
              "Please choose a column to use to test your regular expression:", 
              choices = colnames(clinical_vals$clinical_data))
})

output$col_match_results <- renderDT({
  datatable(match_results(), options = list(dom = "t", scrollY = 300, paging = FALSE), rownames = FALSE)
})

match_results <- eventReactive(input$test_regex, {
  matches <- sapply(clinical_vals$clinical_data[,input$col_to_test], function(x) {
    length(str_extract_all(x, input$regex_to_test)[[1]])
  })
  result <- cbind(clinical_vals$clinical_data[,input$col_to_test], matches)
  colnames(result) <- c(input$col_to_test, "Num.Matches")
  result
})