tabPanel(title = icon(""), value = "1", 
         h4("Matching up datasets"),
         p(paste0("Here is where you can make sure the assay data only includes the samples listed",
                  " in the clinical data (and visa versa) and the assay data only has the probes listed in the ",
                  "feature data (and visa versa). Please note that this will not combine any of these datasets, ",
                  "only filter the rows so that the data is concordant.")),
         selectInput("data_to_match1", "Choose the first dataset to match:", choices = c("clinical", "assay", "feature")),
         selectInput("col_to_match1", "Choose the column from the first dataset to match:", choices = c("col1", "col2", "col3")),
         selectInput("data_to_match2", "Choose the second dataset to match:", choices = c("clinical", "assay", "feature")),
         selectInput("col_to_match2", "Choose the column from the second dataset to match:", choices = c("col1", "col2", "col3")),
         actionButton("match_columns", "Match")
  
)