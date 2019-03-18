tabPanel("1",
         h4("Selecting informative columns"),
         p("It can be helpful to filter out unneeded columns for better storage capacity and improved
           human readability. Here, you can choose which columns are most important for you to keep
           and drop the rest. You may either use preset filters that will detect commonly-dropped
           columns, or select specific columns to keep."),
         radioButtons(inputId = "filter_option", label = div("Please select an option:", help_link("filter_help")),
                      choices = c("Use preset filters" = "preset_filters",
                                  "Select columns by column name" = "column_filters")),
         conditionalPanel(condition = "input.filter_option == 'preset_filters'",
                          checkboxGroupInput(inputId = "download_data_filter", label = div("Remove columns in which every value...",
                                                                                           help_button("Drops columns that match the selected criteria from the table.")),
                                             choiceNames = list("Is the same", 
                                                                "Is unique",
                                                                "Contains a date", 
                                                                "Is a web address"),
                                             choiceValues = list("same_vals", "all_diff", "dates", "url"))
         ),
         conditionalPanel(condition = "input.filter_option == 'column_filters'",
                          div(tags$b("Choose columns to keep:"),
                              help_button("This will drop unselected columns from the table.")),
                          checkboxInput(inputId = "select_all_columns", label = tags$i("Select all"), value = TRUE),
                          uiOutput("display_vars_to_keep")
         ),
         primary_button(id = "clinical_evaluate_filters", label = "Filter columns"),
         hr(), uiOutput("nav_1_ui")
         )