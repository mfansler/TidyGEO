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
    br(),
    selectizeInput(inputId = "geoID", label = div("Please input a GSE ID: ", 
                                                  help_link("choose", "download_help")), choices = NULL),
    uiOutput("platform_options"),
    primary_button(id = "download_data_evaluate", label = div(icon("sign-in-alt"), "Import")),
    hr(), 
    
    div(
      secondary_button('nav_choose_to_clinical_button', div(icon("clipboard"), HTML('Process <br/> clinical data'))),
      tertiary_button('nav_choose_to_assay_button', div(icon("microscope"), HTML('Process <br/> assay data')), class = "right_align"),
      br()
    )#,
    #uiOutput("start_clinical_nav_ui")
  ),
  mainPanel(
    bsAlert("alert"),
    uiOutput("series_information"),
    uiOutput("paper_information")
  )
)