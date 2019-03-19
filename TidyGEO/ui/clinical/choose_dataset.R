sidebarLayout(
  sidebarPanel(
    h4("Importing the data"),
    div("Welcome to TidyGEO! This application allows you to reformat data
        from ",
        a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/", "Gene Expression Omnibus,"),
        " which can then be used to answer research questions. Once you have found a",
        a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/gds", "series of interest,"),
        "complete the instructions below."
    ),
    selectizeInput(inputId = "geoID", label = div("Please input a GSE ID: ", 
                                                  help_link(id = "download_help")), choices = NULL),
    uiOutput("platform_options"),
    primary_button(id = "download_data_evaluate", label = "Import"),
    hr(), uiOutput("start_clinical_nav_ui")
  ),
  mainPanel(
    h4("Series information"),
    bsAlert("alert"),
    uiOutput("series_information_description"),
    htmlOutput("series_information")
  )
)