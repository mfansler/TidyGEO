tabPanel(title = icon("th-list"), value = "3",
         h4("Splitting key-value pairs"),
         p('Sometimes a single column contains key-value pairs in each cell. This makes it more difficult to analyze the data.'),
         p('If you see any columns in your data that contain key-value pairs separated by a delimiter
           (e.g., "treatment:control" or "sex = female"), 
           you can indicate that here and separate them.'),
         div(tags$b("Please select columns that contain key-value pairs:"),
             help_link(id = "split_help_feature")),
         checkboxInput(inputId = "select_all_split_feature", 
                       label = tags$i("Select all")),
         uiOutput("choose_cols_to_split_feature"),
         textInput(inputId = "split_delimiter_feature", label = "Delimiter (including any spaces): ", 
                   placeholder = "Start typing..."),
         checkboxInput("split_pairs_w_regex_feature", div(
           "Use regex",
           actionLink(inputId = "regex_help_split_pairs_feature", label = div(
             tags$i("Help/testing"),
             icon("question-circle")
           ))
         )), 
         div(
           primary_button(id = "split_pairs_feature", label = div(icon("th-list"), "Split pairs")),
           tipify(tertiary_button(id = "undo_split_pairs_feature", label = div(icon("undo"), "Undo"), class = "right_align"), title = "Undo the last action.", placement = "bottom", trigger = "hover")
         ),
         hr(), navigation_set("2", "3", "4", "expression_side_panel", "expression_side_panel"))