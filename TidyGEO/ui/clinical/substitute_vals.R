tabPanel("4",
         h4("Substituting values"),
         p("In order to achieve the uniformity required to combine datasets, it may be helpful to substitute 
           some of the values in the data for other values. Here, you can identify values you would like to replace
           and an alternative to replace them with."),
         uiOutput("display_cols_to_sub"),
         checkboxInput(inputId = "substitute_isrange", 
                       label = div("Specify a range of values to substitute?", 
                                   help_button("Helpful for numeric data."))),
         conditionalPanel(condition = "input.substitute_isrange == true",
                          uiOutput("input_sub_range")),
         h5('Right click to add or remove rows.'),
         rHandsontableOutput("input_subs_table"),
         conditionalPanel(condition = "input.substitute_isrange == false",
                          checkboxInput("sub_w_regex", div("Use regex",
                                                           help_link(id = "regex_help"))))
         ,
         primary_button("evaluate_subs", "Substitute"),
         hr(), uiOutput("nav_4_ui")
)