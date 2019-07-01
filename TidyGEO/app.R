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

version <- suppressWarnings(readLines("VERSION"))

# colored buttons of different types --------------------------------------

source(file.path("ui", "button_types.R"), local = TRUE)$value

options(shiny.autoreload = F)



# UI ----------------------------------------------------------------------


ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "icon", type = "image/png", href = "logo_icon.png")
    ),
  includeScript("www/reactive_preferences.js"),
  useShinyjs(),
  
  navbarPage(title = "TidyGEO", id = "top_level", collapsible = TRUE,
             tabPanel(title = "Choose dataset", value = "choose_dataset",
                      source(file.path("ui", "clinical", "choose_dataset.R"), local = TRUE)$value
             ),
             
             
             # ** clinical data -----------------------------------------------------------
             
             tabPanel(title = div(icon("clipboard"), "Clinical data"), value = "clinical_data",
                      sidebarLayout(
                        
                        # ** ** side panel --------------------------------------------------------------
                        sidebarPanel(
                          tabsetPanel(id = "clinical_side_panel",
                                      source(file.path("ui", "clinical", "1_select_cols.R"), local = TRUE)$value,
                                      source(file.path("ui", "clinical", "2_shift_cells.R"), local = TRUE)$value,
                                      source(file.path("ui", "clinical", "3_split_pairs.R"), local = TRUE)$value,
                                      source(file.path("ui", "clinical", "4_split_cols.R"), local = TRUE)$value,
                                      source(file.path("ui", "clinical", "5_rename_cols.R"), local = TRUE)$value,
                                      source(file.path("ui", "clinical", "6_substitute_vals.R"), local = TRUE)$value,
                                      source(file.path("ui", "clinical", "7_filter_vals.R"), local = TRUE)$value,
                                      source(file.path("ui", "clinical", "8_save_data.R"), local = TRUE)$value
                          ) # clinical tabpanel
                        ), # clinical sidebarpanel

                        # ** ** main panel --------------------------------------------------------------
                        mainPanel(
                          tabsetPanel(
                            source(file.path("ui", "clinical", "display_data.R"), local = TRUE)$value,
                            source(file.path("ui", "clinical", "graphical_summary.R"), local = TRUE)$value
                          ) # tabset panel in main panel
                        ) # clinical main panel
                      ) #sidebar layout
             ), #clinical data tab panel
             
             
             # ** assay data ---------------------------------------------------------
             
             tabPanel(title = div(icon("microscope"), "Assay data"), value = "assay_data",

                      # ** ** side panel --------------------------------------------------------------
                      sidebarLayout(
                          source(file.path("ui", "assay", "side_panel.R"), local = TRUE)$value, # sidebarPanel

                      # ** ** main panel --------------------------------------------------------------
                        mainPanel(
                          tabsetPanel(
                            source(file.path("ui", "assay", "display_data.R"), local = TRUE)$value,
                            source(file.path("ui", "assay", "graphical_summary.R"), local = TRUE)$value
                          )
                        ) # main panel
                      ) # sidebar layout
             ), # expression data tab panel
             

            # ** feature data ------------------------------------------------------------

            tabPanel(title = div(icon("dna"), "Feature data"), value = "feature_data",
                     sidebarLayout(
                       sidebarPanel(),
                       mainPanel()
                     )),

            # ** all data ----------------------------------------------------------------

            tabPanel(title = div(icon("project-diagram"), "Integrate data"), value = "integrate_data",
                      sidebarLayout(
                        sidebarPanel(),
                        mainPanel()
                      )),
            
            
             # ** FAQ ---------------------------------------------------------------------

             tabPanel(title = "FAQ",
                      includeMarkdown("help_docs/FAQ.md")
             ),
             

             # ** about page --------------------------------------------------------------

             tabPanel(title = "About",
                      h2(paste("Version:", version)),
                      includeMarkdown("help_docs/About.md")
              )
  ) #master panel
) #fluidPage



# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  #look for setting to get it do not disconnect
  session$allowReconnect(TRUE)
  session$onSessionEnded(stopApp)

  
# ** reactive values ---------------------------------------------------------

  values <-
    reactiveValues(
      allData = NULL,
      display_barwidth_option = FALSE,
      errorState = FALSE,
      series_needs_download = FALSE,
      series_information = NULL,
      paper_info_expanded = FALSE,
      paper_information = NULL,
      pm_id = NULL
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
      plot_to_save = NULL,
      shift_results = list()
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
      expression_warning_state = FALSE,
      data_to_display = NULL,
      shift_results = NULL
    )
  
  feature_vals <-
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
      expression_warning_state = FALSE,
      data_to_display = NULL,
      shift_results = NULL
    )
  
  integrate_vals <- reactiveValues()
  
  source(file.path("server", "clinical", "choose_dataset.R"), local = TRUE)$value


  # ** clinical data -----------------------------------------------------------

  # ** ** reset ----------------------------------------------------------------
  observeEvent(input$reset, {
    clinical_vals$clinical_data <- clinical_vals$orig_data
    clinical_vals$oFile <- removeFromScript(clinical_vals$oFile, len = clinical_vals$download_chunk_len, all = T)
    clinical_vals$current_chunk_len <- 0
  })  
  
  # ** ** undo --------------------------------------------------------------------
  undo_last_action <- function() {
    clinical_vals$clinical_data <- clinical_vals$last_data
    clinical_vals$oFile <- removeFromScript(clinical_vals$oFile, len = clinical_vals$current_chunk_len)
    clinical_vals$current_chunk_len <- 0
  }

  # ** ** side panel --------------------------------------------------------------
  source(file.path("server", "clinical", "1_select_cols.R"), local = TRUE)$value
  source(file.path("server", "clinical", "2_shift_cells.R"), local = TRUE)$value
  source(file.path("server", "clinical", "3_split_pairs.R"), local = TRUE)$value
  source(file.path("server", "clinical", "4_split_cols.R"), local = TRUE)$value
  source(file.path("server", "clinical", "5_rename_cols.R"), local = TRUE)$value
  source(file.path("server", "clinical", "6_substitute_vals.R"), local = TRUE)$value
  source(file.path("server", "clinical", "7_filter_vals.R"), local = TRUE)$value
  source(file.path("server", "clinical", "8_save_data.R"), local = TRUE)$value

  # ** ** main panel --------------------------------------------------------------
  source(file.path("server", "clinical", "display_data.R"), local = TRUE)$value
  source(file.path("server", "clinical", "graphical_summary.R"), local = TRUE)$value
  
    
  # ** assay data -----------------------------------------------------------------

  # ** ** assay formatting --------------------------------------------------------
  source(file.path("server", "assay", "side_panel.R"), local = TRUE)$value
  source(file.path("server", "assay", "format_data.R"), local = TRUE)$value
  source(file.path("server", "assay", "save_data.R"), local = TRUE)$value

  # ** ** feature formatting ------------------------------------------------------
  source(file.path("server", "feature", "feature_info.R"), local = TRUE)$value
  source(file.path("server", "feature", "shift_cells.R"), local = TRUE)$value
  source(file.path("server", "feature", "split_pairs.R"), local = TRUE)$value
  source(file.path("server", "feature", "save_data.R"), local = TRUE)$value
  
  # ** ** main panel --------------------------------------------------------------
  source(file.path("server", "assay", "display_data.R"), local = TRUE)$value
  source(file.path("server", "assay", "graphical_summary.R"), local = TRUE)$value
  
  
  # ** navigation --------------------------------------------------------------
  
  source(file.path("server", "navigation.R"), local = TRUE)$value
  
  
  # ** help modals -------------------------------------------------------------
  
  source(file.path("server", "help_modals.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
