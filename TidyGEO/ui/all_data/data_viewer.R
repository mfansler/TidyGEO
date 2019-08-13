tabPanel(title = "View datasets", value = "2",
  br(),
  selectInput("data_to_view", "Please select the dataset you would like to view:", choices = c("clinical", "assay", "feature", "all")),
  uiOutput("view_data")
)
