tabPanel(title = FILTER_ICON, value = "1",
         h4("Selecting informative columns"),
         p("It can be helpful to filter out unneeded columns for better storage capacity and improved
           human readability."), 
         p("Here, you can choose which columns are most important for you to keep
           and drop the rest. You may either use preset filters that will detect commonly-dropped
           columns, or select specific columns to keep."),
         radioButtons(inputId = "filter_option", label = div("Please select an option:", help_link("clinical", "filter_help")),
                      choices = c("Use preset filters (see options below)" = "preset_filters",
                                  "Select columns by column name" = "column_filters")),
         help_modal_ui("clinical_filter_help", "Selecting informative columns", "help_docs/Filter_Data_Documentation.md", c("filter_presets_example.gif", "filter_by_name_example.gif"), c("Demo - Filter using Presets", "Demo - Filter by Name")),
         conditionalPanel(condition = "input.filter_option == 'preset_filters'",
                          div(tags$b("Preset filters"),
                              help_button("Drops columns that match the selected criteria from the table.")),
                          tags$i("Remove columns in which every value..."),
                          checkboxGroupInput(inputId = "download_data_filter", label = NULL,
                                             choiceNames = list("Is the same", 
                                                                "Is unique",
                                                                "Contains a date", 
                                                                "Is a web address"),
                                             choiceValues = list("same_vals", "all_diff", "dates", "url"))
         ),
         conditionalPanel(condition = "input.filter_option == 'column_filters'",
                          div(tags$b("By column name"),
                              help_button("This will drop unselected columns from the table.")),
                          tags$i("Keep the following columns..."),
                          checkboxInput(inputId = "select_all_columns", label = tags$i("Select all"), value = TRUE),
                          #checkboxGroupInput(inputId = "varsToKeep", label = NULL)
                          uiOutput("display_vars_to_keep")
         ),
         div(
           primary_button(id = "clinical_evaluate_filters", label = div(FILTER_ICON, "Filter columns")),
           undo_button("undo_select")
         ),
         hr(), tertiary_button(nav("clinical", "choose"), div(PREV_ICON, "Back to choose")),
         secondary_button(nav("1", "2", "clinical"), div("Next", NEXT_ICON), class = "right_align")
)