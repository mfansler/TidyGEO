tabPanel(title = icon("angle-double-right"), value = "2",
         h4("Shifting cells"),
         p("Sometimes, missing values can cause columns to break and shift over, so that the values are listed under
           the wrong column name", actionLink(inputId = "show_broken_cols_example", label = "(example).")),
         p("Here, you can identify a column whose values you would like to shift into a different column.",
             help_link(id = "shift_help")),
         uiOutput("display_cols_to_shift"),
         uiOutput("display_destination_cols"),
         h5('Here is a preview of the first 5 rows of the columns you have selected.'),
         wellPanel(DTOutput("shift_preview_table")),
         div(
           primary_button("evaluate_shift", div(icon("angle-double-right"), "Shift cells")),
           tipify(tertiary_button(id = "undo_shift", label = div(icon("undo"), "Undo"), class = "right_align"), title = "Undo the last action.", placement = "bottom", trigger = "hover")
         ),
         hr(), uiOutput("nav_2_ui")
         )