tabPanel(title = icon("pencil-alt"), value = "5",
         h4("Renaming columns"),
         p("In order to integrate the data with other data sources or for humans to be able to understand the data,
           it may be helpful to replace the existing column names with more accurate/descriptive ones."),
         p("Here, you can give any column a new name."),
         uiOutput("display_cols_to_rename"),
         textInput(inputId = "rename_new_name", label = "Please specify a new name for the column."),
         div(
           primary_button(id = "rename", label = div(icon("pencil-alt"), "Rename column")),
           tipify(tertiary_button(id = "undo_rename", label = div(icon("undo"), "Undo"), class = "right_align"), title = "Undo the last action.", placement = "bottom", trigger = "hover")
         ),
         hr(), navigation_set("4", "5", "6", "clinical_side_panel", "clinical_side_panel")
)