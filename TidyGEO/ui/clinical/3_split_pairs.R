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
                                                            help_link("clinical", "split_help")),
         #conditionalPanel(condition = "input.to_split == true",
                          checkboxInput(inputId = "select_all_split", 
                                        label = tags$i("Select all")),
                          uiOutput("choose_cols_to_split"),
                          textInput(inputId = "split_delimiter", label = "Delimiter (including any spaces): ", placeholder = "Start typing...")
         #)
         ,
         checkboxInput("split_pairs_w_regex", div(
           "Use regex", regex_help_link("split_pairs")
         )), 
         div(
           primary_button(id = "split_pairs", label = div(icon("th-list"), "Split pairs")),
           undo_button("undo_split_pairs")
         ),
         hr(), navigation_set("2", "3", "4", "clinical_side_panel", "clinical_side_panel")
)