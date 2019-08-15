tabPanel(title = icon("project-diagram"), value = "2",
  h4("Joining datasets"),
  p(paste0("Here is where you can join clinical, assay, and/or feature data, essentially merging the two datasets ",
           "into one. Please note that this may produce an unwieldy dataset that may not be able to be loaded into ",
           "Excel.")),
  selectInput("data_to_join1", "Choose the first dataset to join:", choices = c("clinical", "assay", "feature")),
  uiOutput("col_to_join1_selector"),
  primary_button("add_dataset", "Add dataset"),
  disabled(secondary_button("remove_dataset", "Remove last", class = "right_align")),
  br(),
  br(),
  primary_button("join_columns", div(icon("project-diagram"), "Join")),
  hr(), navigation_set("1", "2", "3", "all_data_options", "all_data_options")
)