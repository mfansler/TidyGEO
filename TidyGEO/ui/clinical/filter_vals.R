tabPanel("5",
         h4("Filtering samples"),
         p("You may want to remove some of the values in a column, for example, if you have missing (NA) values."), 
         p("Here, you can specify which values you would like to remove.
           Excluding a value will take out the entire row that contains that value in the selected column."),
         uiOutput("display_cols_for_exclude"),
         checkboxInput("exclude_isrange", 
                       label = div("Select a range (for numeric data)", 
                                   help_button("Excludes/keeps all numeric values in a range."))),
         conditionalPanel(condition = "input.exclude_isrange == true",
                          uiOutput("sliderExclude")),
         conditionalPanel(condition = "input.exclude_isrange == false",
                          div(tags$b("Which values would you like to exclude?"), 
                              help_button("Excluding a value will remove the entire row that contains that value.")),
                          checkboxInput(inputId = "select_all_exclude", label = tags$i("Select all")),
                          uiOutput("display_vals_to_exclude")),
         
         div(
           primary_button("clinical_evaluate_exclude", div(icon("times"), "Exclude")),
           tipify(tertiary_button(id = "undo_filter", label = div(icon("undo"), "Undo"), class = "right_align"), title = "Undo the last action.", placement = "bottom", trigger = "hover")
         ),
         hr(), uiOutput("nav_5_ui")
)