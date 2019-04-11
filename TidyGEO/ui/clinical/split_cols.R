tabPanel("2",
         h4("Formatting the data"),
         p('Sometimes a single column contains key-value pairs or multiple values in each cell. This makes it more difficult to analyze the data.'),
         p('If you see any columns in your data that contain key-value pairs 
          (e.g., "treatment:control") or multiple values in a column (e.g., "control;geneA;female"), 
           you can indicate that here and separate them.'),
         checkboxInput(inputId = "to_split", label = div(tags$b("Modify columns with key-value pairs separated by a delimiter"),
                                                         help_link(id = "split_help")
         )),
         conditionalPanel(condition = "input.to_split == true",
                          checkboxInput(inputId = "select_all_split", 
                                        label = tags$i("Select all")),
                          uiOutput("choose_cols_to_split"),
                          textInput(inputId = "split_delimiter", label = "Delimiter (including any spaces): ")
         ),
         checkboxInput(inputId = "to_divide", label = div(tags$b("Modify columns with multiple values in one column"),
                                                          help_link(id = "divide_help"))),
         conditionalPanel(condition = "input.to_divide == true",
                          checkboxInput(inputId = "select_all_divide", 
                                        label = tags$i("Select all")),
                          uiOutput("choose_cols_to_divide"),
                          textInput(inputId = "divide_delimiter", label = "Delimiter (including any spaces): ")
         ),
         div(
           primary_button(id = "reformat_columns", label = div(icon("columns"), "Reformat columns")),
           tipify(tertiary_button(id = "undo_reformat", label = div(icon("undo"), "Undo"), class = "right_align"), title = "Undo the last action.", placement = "bottom", trigger = "hover")
         ),
         hr(), uiOutput("nav_2_ui")
)