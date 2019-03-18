suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(shinycssloaders)
  library(shinyBS)
  library(plotly)
  library(feather)
  library(shinyjs)
  library(rhandsontable)
  library(shinyWidgets)
  library(RColorBrewer)
  source("tidygeo_functions.R")
})

source(file.path("server", "clinical", "helper_functions.R"), local = TRUE)$value
source(file.path("server", "assay", "helper_functions.R"), local = TRUE)$value

start_time <- Sys.time()
series_list <- read_feather("www/series_list.feather")
platform_list <- read_feather("www/platform_list.feather")
end_time <- Sys.time()
print(paste("Reading files", end_time - start_time))

# help icons to add as tag to buttons, etc --------------------------------

source(file.path("ui", "help_ui.R"), local = TRUE)$value


# colored buttons of different types --------------------------------------

source(file.path("ui", "button_types.R"), local = TRUE)$value

options(shiny.autoreload = F)

# UI ----------------------------------------------------------------------


ui <- fluidPage(
  #centers loading bars in the middle of the page
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    includeScript("reactive_preferences.js"),
  navbarPage(title = "TidyGEO", id = "top_level",
             tabPanel(title = "Choose dataset",
                      source(file.path("ui", "clinical", "choose_dataset.R"), local = TRUE)$value
             ),
             tabPanel(title = "Clinical data",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          useShinyjs(),
                          tabsetPanel(id = "clinical_side_panel",

                                      # exclude vars ------------------------------------------------------------

                                      source(file.path("ui", "clinical", "select_cols.R"), local = TRUE)$value,
                                      
                                      
                                      # split variables ---------------------------------------------------------
                                      
                                      source(file.path("ui", "clinical", "split_cols.R"), local = TRUE)$value,
                                      
                                      
                                      # rename columns ----------------------------------------------------------
                                      
                                      source(file.path("ui", "clinical", "rename_cols.R"), local = TRUE)$value,
                                      
                                      
                                      # substitute --------------------------------------------------------------
                                      
                                      source(file.path("ui", "clinical", "substitute_vals.R"), local = TRUE)$value,
                                      
                                      
                                      # exclude variables -------------------------------------------------------
                                      
                                      source(file.path("ui", "clinical", "filter_vals.R"), local = TRUE)$value,
                                      

                                      # save clinical data ------------------------------------------------------

                                      source(file.path("ui", "clinical", "save_data.R"), local = TRUE)$value
                                      
                          )
                        ),
                        
                        # display metadata --------------------------------------------------------
                        
                        
                        mainPanel(
                          tabsetPanel(
                            source(file.path("ui", "clinical", "display_data.R"), local = TRUE)$value,
                            source(file.path("ui", "clinical", "graphical_summary.R"), local = TRUE)$value
                          ) #tab panel in main panel
                        ) #main panel
                      ) #sidebar layout
             ), #metadata tab panel
             
             # expression data ---------------------------------------------------------
             
             tabPanel(title = "Assay data",
                      
                      sidebarLayout(
                        sidebarPanel(
                          tabsetPanel(id = "expression_side_panel",
                                      source(file.path("ui", "assay", "format_data.R"), local = TRUE)$value,
                                      source(file.path("ui", "assay", "save_data.R"), local = TRUE)$value
                              ) #tabsetPanel
                          ), #sidebarPanel
                        mainPanel(
                          tabsetPanel(
                            source(file.path("ui", "assay", "display_data.R"), local = TRUE)$value,
                            source(file.path("ui", "assay", "graphical_summary.R"), local = TRUE)$value
                          )
                        ) #main panel
                      ) #sidebar layout
             ), # expression data tab panel
             tabPanel(title = "FAQ",
                      includeMarkdown("www/FAQ.md")
             )
  ) #master panel
) #fluidPage

# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  #look for setting to get it do not disconnect
  session$allowReconnect(TRUE)
  session$onSessionEnded(stopApp)

# reactive values ---------------------------------------------------------

  
  values <-
    reactiveValues(
      allData = NULL,
      default_metaData = data.frame("Please load some clinical data"),
      metaData = NULL,
      origData = NULL,
      lastData = NULL,
      default_expr_data = data.frame("Please load some assay data"),
      orig_expr = NULL,
      expr_data = NULL,
      expr_to_display = NULL,
      default_ft_data = data.frame("Please load some assay data"),
      orig_feature = NULL,
      last_feature = NULL,
      feature_data = NULL,
      feature_to_display = NULL,
      to_split_selected = FALSE,
      last_selected_rename = NULL,
      last_selected_substitute = NULL,
      last_selected_exclude = NULL,
      newName = NULL,
      newNames = NULL,
      NAvalsList = list(),
      DFIn = data.frame(),
      DFOut = data.frame(
        To_Replace = "",
        New_Val = "",
        stringsAsFactors = FALSE
      ),
      tablesList = list(),
      thes_suggest = c("no suggestions"),
      thes_suggest_vals = c("no suggestions"),
      suggestions = c("no suggestions"),
      excludesList = list(),
      oFile = commentify(" "),
      expression_oFile = commentify(" "),
      downloadChunkLen = 0,
      currChunkLen = 0,
      expression_downloadChunkLen = 0,
      expression_currChunkLen = 0,
      subAllNums = F,
      expression_id_col = "ID",
      expression_prev_id = "ID",
      feature_id_col = "ID",
      feature_prev_id = "ID",
      clinical_plot_to_save = NULL,
      expr_plot_to_save = NULL,
      expression_disable_btns = FALSE,
      expression_warning_state = FALSE,
      display_barwidth_option = FALSE
    )
  
  get_series_information <- function() {
    if (!is.null(input$geoID) && input$geoID != "") {
      geo_url <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", input$geoID)
    } else {
      geo_url <- "https://www.ncbi.nlm.nih.gov/gds/"
    }
    return(tags$iframe(src = geo_url, style = "width:100%;",
                       frameborder = "0",
                       id = "iframe", 
                       height = "500px"))
  }
  
  output$series_information <- renderUI({
    get_series_information()
  })
  
  # reset -------------------------------------------------------------------
  
  observeEvent(input$reset, {
    values$metaData <- clinical_vals$orig_data
    clinical_vals$oFile <- removeFromScript(clinical_vals$oFile, len = clinical_vals$download_chunk_len, all = T)
    clinical_vals$current_chunk_len <- 0
  })  
  
  # undo --------------------------------------------------------------------
  
  observeEvent(input$undo, {
    values$metaData <- clinical_vals$last_data
    clinical_vals$oFile <- removeFromScript(clinical_vals$oFile, len = clinical_vals$current_chunk_len)
    clinical_vals$current_chunk_len <- 0
  })  
  
  
  # download & display metaData ---------------------------------------------
  
  source(file.path("server", "clinical", "choose_dataset.R"), local = TRUE)$value
  
  source(file.path("server", "clinical", "display_data.R"), local = TRUE)$value
  
  # summary -----------------------------------------------------------------
  
  source(file.path("server", "clinical", "graphical_summary.R"), local = TRUE)$value
  
  
  # extract columns ---------------------------------------------------------
  
  source(file.path("server", "clinical", "split_cols.R"), local = TRUE)$value
  
  
  # filter columns ----------------------------------------------------------
  
  source(file.path("server", "clinical", "select_cols.R"), local = TRUE)$value

  
  # rename columns ----------------------------------------------------------
  
  source(file.path("server", "clinical", "rename_cols.R"), local = TRUE)$value
  
  
  # substitute vals ---------------------------------------------------------
  
  source(file.path("server", "clinical", "substitute_vals.R"), local = TRUE)$value
  
  # exclude vals ------------------------------------------------------------
  
  source(file.path("server", "clinical", "filter_vals.R"), local = TRUE)$value

  
  # download data -----------------------------------------------------------
  
  source(file.path("server", "clinical", "save_data.R"), local = TRUE)$value
  
  # navigation --------------------------------------------------------------
  
  source(file.path("server", "navigation.R"), local = TRUE)$value
  
  # help modals -------------------------------------------------------------

  source(file.path("server", "help_modals.R"), local = TRUE)$value

  # expression data sidebar -------------------------------------------------
   
  source(file.path("server", "assay", "format_data.R"), local = TRUE)$value
  
  # download expression data -----------------------------------------------------------
  
  source(file.path("server", "assay", "save_data.R"), local = TRUE)$value
  
  # main panel expression data ----------------------------------------------
  
  source(file.path("server", "assay", "display_data.R"), local = TRUE)$value

  # graphical summary -------------------------------------------------------
  
  source(file.path("server", "assay", "graphical_summary.R"), local = TRUE)$value
  
}

shinyApp(ui = ui, server = server)
