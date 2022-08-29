tabPanel(title = icon("align-justify", lib="glyphicon"), value = "5",
         h4("Splitting columns"),
         p('Sometimes a single column contains multiple values in each cell. This makes it more difficult to analyze the data.'),
         p('If you see any columns in your data that contain multiple values in a column (e.g., "control;geneA;female" or "time 0, treatment1"), 
           you can indicate that here and separate them.'),
         #checkboxInput(inputId = "to_divide", label = div(tags$b("Modify columns with multiple values in one column"),
         #                                                  help_link(id = "divide_help"))),
         div(tags$b("Please select columns with multiple values in one column"),
             help_link("feature", "divide_help")),
         #conditionalPanel(condition = "input.to_divide == true",
         checkboxInput(inputId = "feature_select_all_divide", 
                       label = tags$i("Select all")),
         uiOutput("feature_choose_cols_to_divide"),
         textInput(inputId = "feature_divide_delimiter", label = "Delimiter (including any spaces): ", placeholder = "Start typing...")
         #)
         ,
         checkboxInput("feature_split_cols_w_regex", div(
           "Use regex", regex_help_link("feature", "split_cols")
         )), 
         div(
           primary_button(id = "feature_split_cols", label = div(icon("align-justify", lib="glyphicon"), "Split columns")),
           undo_button("feature_undo_split_cols")
         ),
         hr(), navigation_set("4", "5", "6", "feature_side_panel", "feature_side_panel")
         )