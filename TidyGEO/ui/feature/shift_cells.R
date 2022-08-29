tabPanel(title = icon("angles-right"), value = "3",
  h4("Shifting cells"),
  p('Sometimes, missing values can cause data from a single variable (such as "Sex" and "Freq" in ', 
    actionLink(inputId = "show_broken_cols_example_feature", label = "this example)"),
    'to be scattered across multiple columns. This makes it difficult to extract data for the single variable.'),
  p("Here, you can identify a column whose values you would like to shift to a single column. Usually it is best
    to start at the rightmost column that has shifted values and work your way to the left.",
    help_link("feature", "shift_help")),
  uiOutput("display_cols_to_shift_feature"),
  uiOutput("display_destination_cols_feature"),
  HTML('Here is a preview of the first 5 rows of the columns you have selected. Conflicts, where one value might
       be overwritten by another value, are shown in <font color="red">red.</font> You will be given an opportunity
       to resolve these conflicts when you click "Shift cells".'),
  wellPanel(DTOutput("shift_preview_table_feature")),
  div(
    primary_button("evaluate_shift_feature", div(icon("angles-right"), "Shift cells")),
    undo_button("undo_shift_feature")
  ),
  hr(), navigation_set("2", "3", "4", "feature_side_panel", "feature_side_panel")
)