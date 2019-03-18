tabPanel("5",
         h4("Filtering samples"),
         p("You may want to remove some of the values in a column, for example, if you have missing (NA) values.
           Here, you can specify which values you would like to remove.
           Excluding a value will take out the entire row that contains that value in the selected column."),
         uiOutput("display_cols_for_exclude"),
         checkboxInput("exclude_isrange", 
                       label = div("Specify a range of values to exclude?", 
                                   help_button("Helpful for numeric data."))),
         conditionalPanel(condition = "input.exclude_isrange == true",
                          uiOutput("sliderExclude")),
         conditionalPanel(condition = "input.exclude_isrange == false",
                          div(tags$b("Which variables would you like to exclude?"), help_button("Excluding a variable will remove the entire row that contains that variable.")),
                          checkboxInput(inputId = "select_all_exclude", label = tags$i("Select all")),
                          uiOutput("display_vals_to_exclude")),
         primary_button("clinical_evaluate_exclude", "Exclude"),
         hr(), uiOutput("nav_5_ui")
)