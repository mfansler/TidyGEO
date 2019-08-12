tabPanel(title = icon("exchange-alt"), value = "6",
         h4("Substituting values"),
         p("In order to achieve the uniformity required to combine datasets, it may be helpful to substitute 
           some of the values in the data for other values."), 
         p("Here, you can identify values you would like to replace and an alternative to replace them with."),
         uiOutput("display_cols_to_sub"),
         #checkboxInput(inputId = "substitute_isrange", 
        #               label = div("Select a range (for numeric data)", 
        #                           help_button("Discretizes data by replacing all values within a numeric range."))),
         #conditionalPanel(condition = "input.substitute_isrange == true",
                          uiOutput("input_sub_range"),#),
         h5(paste0('Right click to add or remove rows. To delete a word or phrase, ',
                     'enter that word or phrase into the column on the left and leave the column on the right empty.')),
        rHandsontableOutput("input_subs_table"),
        #tags$head(tags$style("#input_subs_table{display:block; width:100%;}")),
        checkboxInput("sub_w_regex", div("Use regex", regex_help_link("substitute"))), 
         div(
           primary_button("evaluate_subs", div(icon("exchange-alt"), "Substitute")),
           undo_button("undo_subs")
         ),
         hr(), navigation_set("5", "6", "7", "clinical_side_panel", "clinical_side_panel")
)