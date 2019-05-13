tabPanel(title = icon("th-list"), value = "3",
         h4("Splitting key-value pairs"),
         p('Sometimes a single column contains key-value pairs in each cell. This makes it more difficult to analyze the data.'),
         p('If you see any columns in your data that contain key-value pairs separated by a delimiter
          (e.g., "treatment:control" or "sex = female"), 
           you can indicate that here and separate them.'),
         #checkboxInput(inputId = "to_split", label = div(tags$b("Modify columns with key-value pairs separated by a delimiter"),
          #                                               help_link(id = "split_help")
         #)),
         div(tags$b("Please select columns that contain key-value pairs:"),
                                                            help_link(id = "split_help")),
         #conditionalPanel(condition = "input.to_split == true",
                          checkboxInput(inputId = "select_all_split", 
                                        label = tags$i("Select all")),
                          uiOutput("choose_cols_to_split"),
                          textInput(inputId = "split_delimiter", label = "Delimiter (including any spaces): ")
         #)
         ,
         checkboxInput("split_pairs_w_regex", div("Use regex",
                                          help_link(id = "regex_help_split_pairs"))),
         div(
           primary_button(id = "split_pairs", label = div(icon("th-list"), "Split pairs")),
           tipify(tertiary_button(id = "undo_split_pairs", label = div(icon("undo"), "Undo"), class = "right_align"), title = "Undo the last action.", placement = "bottom", trigger = "hover")
         ),
         hr(), uiOutput("nav_3_ui")
)