tabPanel(title = icon("columns"), value = "4",
         h4("Splitting columns"),
         p('Sometimes a single column contains multiple values in each cell. This makes it more difficult to analyze the data.'),
         p('If you see any columns in your data that contain multiple values in a column (e.g., "control;geneA;female" or "time 0, treatment1"), 
           you can indicate that here and separate them.'),
         #checkboxInput(inputId = "to_divide", label = div(tags$b("Modify columns with multiple values in one column"),
        #                                                  help_link(id = "divide_help"))),
         div(tags$b("Please select columns with multiple values in one column"),
             help_link(id = "divide_help")),
         #conditionalPanel(condition = "input.to_divide == true",
                          checkboxInput(inputId = "select_all_divide", 
                                        label = tags$i("Select all")),
                          uiOutput("choose_cols_to_divide"),
                          textInput(inputId = "divide_delimiter", label = "Delimiter (including any spaces): ")
         #)
         ,
        checkboxInput("split_cols_w_regex", div(
          "Use regex",
          actionLink(inputId = "regex_help_split_cols", label = div(
            tags$i("Help/testing"),
            icon("question-circle")
          ))
        )), 
         div(
           primary_button(id = "split_cols", label = div(icon("columns"), "Split columns")),
           tipify(tertiary_button(id = "undo_split_cols", label = div(icon("undo"), "Undo"), class = "right_align"), title = "Undo the last action.", placement = "bottom", trigger = "hover")
         ),
         hr(), uiOutput("nav_4_ui")
         )