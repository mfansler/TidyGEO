tabPanel(title = icon("cut"), value = "6",
         h4("Filtering samples"),
         p("You may want to remove some of the samples in the data, for example, if you have samples with missing (NA) values."), 
         p("Here, you can specify values to use as filtering criteria.
           Rows that match the filtering criteria for the specified column will be removed."),
         uiOutput("display_cols_for_exclude"),
         #checkboxInput("exclude_isrange", 
        #               label = div("Select a range (for numeric data)", 
        #                           help_button("Excludes/keeps all numeric values in a range."))),
        # conditionalPanel(condition = "input.exclude_isrange == true",
                          uiOutput("sliderExclude"),#),
         #conditionalPanel(condition = "input.exclude_isrange == false",
        #                  div(tags$b("Which values would you like to exclude?"), 
        #                      help_button("Excluding a value will remove the entire row that contains that value.")),
        #                  checkboxInput(inputId = "select_all_exclude", label = tags$i("Select all")),
        #                  uiOutput("display_vals_to_exclude")),
         
         div(
           primary_button("clinical_evaluate_exclude", div(icon("cut"), "Exclude")),
           tipify(tertiary_button(id = "undo_filter", label = div(icon("undo"), "Undo"), class = "right_align"), title = "Undo the last action.", placement = "bottom", trigger = "hover")
         ),
         hr(), uiOutput("nav_6_ui")
)