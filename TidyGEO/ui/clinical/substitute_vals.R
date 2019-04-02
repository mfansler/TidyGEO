tabPanel("4",
         h4("Substituting values"),
         p("In order to achieve the uniformity required to combine datasets, it may be helpful to substitute 
           some of the values in the data for other values."), 
         p("Here, you can identify values you would like to replace and an alternative to replace them with."),
         uiOutput("display_cols_to_sub"),
         checkboxInput(inputId = "substitute_isrange", 
                       label = div("Select a range (for numeric data)", 
                                   help_button("Discretizes data by replacing all values within a numeric range."))),
         conditionalPanel(condition = "input.substitute_isrange == true",
                          uiOutput("input_sub_range")),
         h5('Right click to add or remove rows.'),
         rHandsontableOutput("input_subs_table"),
         conditionalPanel(condition = "input.substitute_isrange == false",
                          checkboxInput("sub_w_regex", div("Use regex",
                                                           help_link(id = "regex_help")))),
         div(
           primary_button("evaluate_subs", div(icon("exchange-alt"), "Substitute")),
           tipify(tertiary_button(id = "undo_subs", label = div(icon("undo"), "Undo"), class = "right_align"), title = "Undo the last action.", placement = "bottom", trigger = "hover")
         ),
         hr(), uiOutput("nav_4_ui")
)