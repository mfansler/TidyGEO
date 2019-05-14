tabPanel(title = icon("angle-double-right"), value = "2",
         h4("Shifting cells"),
         p('Sometimes, missing values can cause data from a single variable (such as "Sex" and "Freq" in ', 
           actionLink(inputId = "show_broken_cols_example", label = "this example)"),
           'to be scattered across multiple columns. This makes it difficult to extract data for the single variable.'),
         p("Here, you can identify a column whose values you would like to shift back to their proper column.",
             help_link(id = "shift_help")),
         uiOutput("display_cols_to_shift"),
         uiOutput("display_destination_cols"),
         HTML('Here is a preview of the first 5 rows of the columns you have selected. Conflicts, where one value might
            be overwritten by another value, are shown in <font color="red">red.</font>'),
         wellPanel(DTOutput("shift_preview_table")),
         div(
           primary_button("evaluate_shift", div(icon("angle-double-right"), "Shift cells")),
           tipify(tertiary_button(id = "undo_shift", label = div(icon("undo"), "Undo"), class = "right_align"), title = "Undo the last action.", placement = "bottom", trigger = "hover")
         ),
         hr(), uiOutput("nav_2_ui")
         )