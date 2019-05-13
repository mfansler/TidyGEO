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

# colored buttons of different types --------------------------------------

source(file.path("ui", "button_types.R"), local = TRUE)$value

options(shiny.autoreload = F)

# UI ----------------------------------------------------------------------


ui <- fluidPage(
  #centers loading bars in the middle of the page
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "logo_icon.png")
    ),
    includeScript("www/reactive_preferences.js"),
  #titlePanel(#"TidyGEO",
  #           tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.ico.png"),
  #                     tags$title("TidyGEO"))
  #),
  navbarPage(title = "TidyGEO", id = "top_level",
             tabPanel(title = "Choose dataset",
                      source(file.path("ui", "clinical", "choose_dataset.R"), local = TRUE)$value
             ),
             tabPanel(title = div(icon("clipboard"), "Clinical data"), value = "Clinical data",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          useShinyjs(),
                          tabsetPanel(id = "clinical_side_panel",

                                      # exclude vars ------------------------------------------------------------

                                      source(file.path("ui", "clinical", "select_cols.R"), local = TRUE)$value,
                                      

                                      # shift cells -------------------------------------------------------------
                                      
                                      source(file.path("ui", "clinical", "shift_cells.R"), local = TRUE)$value,
                                      
                                      
                                      # split key-value pairs ---------------------------------------------------
                                      
                                      source(file.path("ui", "clinical", "split_pairs.R"), local = TRUE)$value,

                                      
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
             
             tabPanel(title = div(icon("microscope"), "Assay data"), value = "Assay data",
                      
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
                      includeMarkdown("help_docs/FAQ.md")
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
      display_barwidth_option = FALSE,
      errorState = FALSE,
      series_needs_download = FALSE
    )
  
  clinical_vals <- 
    reactiveValues(
      display_default = data.frame("Please load some clinical data"),
      clinical_data = NULL,
      orig_data = NULL,
      last_data = NULL,
      last_selected_rename = NULL,
      last_selected_substitute = NULL,
      last_selected_exclude = NULL,
      subs_input = data.frame(),
      subs_display = data.frame(
        To_Replace = "",
        New_Val = "",
        stringsAsFactors = FALSE
      ),
      oFile = commentify(" "),
      download_chunk_len = 0,
      current_chunk_len = 0,
      plot_to_save = NULL
    )
  
  assay_vals <-
    reactiveValues(
      display_default = data.frame("Please load some assay data"),
      orig_data = NULL,
      assay_data = NULL,
      assay_display = NULL,
      last_data = NULL,
      ft_default = data.frame("Please load some assay data"),
      orig_feature = NULL,
      last_feature = NULL,
      feature_data = NULL,
      feature_display = NULL,
      oFile = commentify(" "),
      download_chunk_len = 0,
      current_chunk_len = 0,
      id_col = "ID",
      prev_id = "ID",
      ft_id_col = "ID",
      ft_prev_id = "ID",
      plot_to_save = NULL,
      disable_btns = FALSE,
      expression_warning_state = FALSE
    )
  
  #get_series_information <- function() {
  #  if (!is.null(input$geoID) && input$geoID != "") {
  #    geo_url <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", input$geoID)
  #  } else {
  #    geo_url <- "https://www.ncbi.nlm.nih.gov/gds/"
  #  }
  #  return(tags$iframe(src = geo_url, style = "width:100%;",
  #                     frameborder = "0",
  #                     id = "iframe", 
  #                     height = "500px"))
  #}
#  
  #output$series_information <- renderUI({
  #  get_series_information()
  #})
  
  # reset -------------------------------------------------------------------
  
  observeEvent(input$reset, {
    clinical_vals$clinical_data <- clinical_vals$orig_data
    clinical_vals$oFile <- removeFromScript(clinical_vals$oFile, len = clinical_vals$download_chunk_len, all = T)
    clinical_vals$current_chunk_len <- 0
  })  
  
  # undo --------------------------------------------------------------------
  
  undo_last_action <- function() {
    clinical_vals$clinical_data <- clinical_vals$last_data
    clinical_vals$oFile <- removeFromScript(clinical_vals$oFile, len = clinical_vals$current_chunk_len)
    clinical_vals$current_chunk_len <- 0
  }
  
  
  # download & display metaData ---------------------------------------------
  
  source(file.path("server", "clinical", "choose_dataset.R"), local = TRUE)$value
  
  source(file.path("server", "clinical", "display_data.R"), local = TRUE)$value
  
  # summary -----------------------------------------------------------------
  
  source(file.path("server", "clinical", "graphical_summary.R"), local = TRUE)$value
  
  
  # extract columns ---------------------------------------------------------
  
  source(file.path("server", "clinical", "split_pairs.R"), local = TRUE)$value
  

  # shift cells -------------------------------------------------------------
  
  source(file.path("server", "clinical", "split_cols.R"), local = TRUE)$value
  
  
  # filter columns ----------------------------------------------------------
  
  source(file.path("server", "clinical", "select_cols.R"), local = TRUE)$value

  
  # rename columns ----------------------------------------------------------
  
  source(file.path("server", "clinical", "rename_cols.R"), local = TRUE)$value
  
  
  # substitute vals ---------------------------------------------------------
  
  source(file.path("server", "clinical", "substitute_vals.R"), local = TRUE)$value
  
  # exclude vals ------------------------------------------------------------
  
  source(file.path("server", "clinical", "filter_vals.R"), local = TRUE)$value
  
  source(file.path("server", "clinical", "shift_cells.R"), local = TRUE)$value

  
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
