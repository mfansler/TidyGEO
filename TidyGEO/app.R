# A NOTE TO PROGRAMMERS WORKING WITH THIS APP
# 
# At first, you will not understand what my functions are doing and you will be tempted to do
# things manually. I STRONGLY DISCOURAGE THIS as I have most things knit together just so.
# Doing things manually will bypass the error checking I have set up and will likely be a
# nightmare to debug. WHENEVER POSSIBLE, use existing functions to perform operations. These
# functions can be found at the beginning of the server function, in tidygeo_functions.R,
# in formatting_helper_functions.R, in help_modals.R, and in button_types.R. If you would like
# to make a change, it would be MOST HELPFUL to change these functions rather than starting
# from scratch.
# 
# Thanks,
# A

source("tidygeo_variables.R")

suppressPackageStartupMessages({
  source("tidygeo_functions.R")
  source("server/formatting_helper_functions.R")
})

# colored buttons of different types --------------------------------------

source(file.path("ui", "button_types.R"), local = TRUE)$value

options(shiny.autoreload = F)

# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "TidyGEO"
  ),
  dashboardSidebar(collapsed = TRUE,
    sidebarMenu(id = "top_level",
      menuItem("Choose dataset", tabName = "choose_dataset"),
      menuItem("Process data", tabName = "process_data",
               menuSubItem("Clinical data", icon = CLINICAL_ICON, tabName = "clinical_data"),
               menuSubItem("Assay data", icon = ASSAY_ICON, tabName = "assay_data"),
               menuSubItem("Feature data", icon = FEATURE_ICON, tabName = "feature_data"),
               menuSubItem("All data", icon = ALL_ICON, tabName = "all_data")),
      menuItem("FAQ", tabName = "faq"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$link(rel = "icon", type = "image/png", href = "logo_icon.png")
    ),
    includeScript("www/reactive_preferences.js"),
    useShinyjs(),
    tabItems(
      tabItem("choose_dataset",
               source(file.path("ui", "clinical", "choose_dataset.R"), local = TRUE)$value
      ),
      tabItem(dataname("clinical"),
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
      
      tabItem(dataname("assay"),
               
               # ** ** side panel --------------------------------------------------------------
               sidebarLayout(
                 #source(file.path("ui", "assay", "side_panel.R"), local = TRUE)$value, # sidebarPanel
                 sidebarPanel(
                   tabsetPanel(id = "expression_side_panel", 
                               source(file.path("ui", "assay", "format_data.R"), local = TRUE)$value,
                               source(file.path("ui", "assay", "save_data.R"), local = TRUE)$value)
                   
                 ),
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
      
      tabItem(dataname("feature"),
               sidebarLayout(
                 sidebarPanel(
                   tabsetPanel(id = "feature_side_panel",
                     source(file.path("ui", "feature", "feature_info.R"), local = TRUE)$value,
                     source(file.path("ui", "feature", "shift_cells.R"), local = TRUE)$value,
                     source(file.path("ui", "feature", "split_pairs.R"), local = TRUE)$value,
                     source(file.path("ui", "feature", "split_cols.R"), local = TRUE)$value,
                     source(file.path("ui", "feature", "save_data.R"), local = TRUE)$value
                   )
                 ),
                 mainPanel(
                   tabsetPanel(
                     source(file.path("ui", "feature", "display_data.R"), local = TRUE)$value,
                     source(file.path("ui", "feature", "graphical_summary.R"), local = TRUE)$value
                   )
                 )
               )),
      # ** all data ----------------------------------------------------------------
      
      tabItem(dataname("all"),
               sidebarLayout(
                 sidebarPanel(
                   tabsetPanel(id = "all_data_options",
                     source(file.path("ui", "all_data", "filter_rows.R"), local = TRUE)$value,
                     source(file.path("ui", "all_data", "join_dfs.R"), local = TRUE)$value,
                     source(file.path("ui", "all_data", "save_data.R"), local = TRUE)$value
                   )
                 ),
                 mainPanel(
                   tabsetPanel(id = "all_data_main_panel",
                     source(file.path("ui", "all_data", "workbench.R"), local = TRUE)$value,
                     source(file.path("ui", "all_data", "data_viewer.R"), local = TRUE)$value
                   )
                 )
               )),
      # ** FAQ ---------------------------------------------------------------------
      
      tabItem("faq",
               includeMarkdown("help_docs/FAQ.md")
      ),
      # ** about page --------------------------------------------------------------
      
      tabItem("about",
               h2(paste("Version:", VERSION)),
               includeMarkdown("help_docs/About.md")
      )
    )
  )
) #fluidPage



# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  #look for setting to get it do not disconnect
  session$allowReconnect(TRUE)
  session$onSessionEnded(stopApp)
  
  source(file.path("tidygeo_server_functions.R"), local = TRUE)$value

  
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
      pm_id = NULL,
      regex_dt = NULL
    )
  assign(
    varname("clinical"),
    DataType("clinical",
             last_selected_rename = NULL,
             last_selected_substitute = NULL,
             last_selected_exclude = NULL,
             subs_input = data.frame(),
             subs_display = data.frame(
               To_Replace = "",
               New_Val = "",
               stringsAsFactors = FALSE
             )
    )
  )
  assign(
    varname("assay"),
    DataType("assay",
             id_col = "ID",
             prev_id = "ID",
             disable_btns = FALSE,
             expression_warning_state = FALSE
    )
  )
  assign(
    varname("feature"),
    DataType("feature",
             shift_results = NULL,
             id_col = "ID",
             prev_id = "ID"
    )
  )
  assign(
    varname("all"),
    DataType("all",
             last_selected_match1 = NULL,
             last_selected_match2 = NULL,
             join_datatypes_visible = 1
    )
  )

  # ** navigation --------------------------------------------------------------
  
  source(file.path("server", "navigation.R"), local = TRUE)$value
  
  
  # ** Choose dataset ----------------------------------------------------------

  source(file.path("server", "clinical", "choose_dataset.R"), local = TRUE)$value

  
  observeEvent(input$top_level, {
    if (!is.null(values$allData)) {
      if (input$top_level == dataname("clinical") && !data_loaded("clinical")) {
        get_clinical_data()
      } else if (input$top_level == dataname("assay") && !data_loaded("assay")) {
        get_assay_data()
      } else if (input$top_level == dataname("feature") && !data_loaded("feature")) {
        get_feature_data()
      }
    }
  })
  
  # ** clinical data -----------------------------------------------------------
  
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

  # ** ** side panel --------------------------------------------------------------
  #source(file.path("server", "assay", "side_panel.R"), local = TRUE)$value
  source(file.path("server", "assay", "format_data.R"), local = TRUE)$value
  source(file.path("server", "assay", "save_data.R"), local = TRUE)$value

  # ** ** main panel --------------------------------------------------------------
  source(file.path("server", "assay", "display_data.R"), local = TRUE)$value
  source(file.path("server", "assay", "graphical_summary.R"), local = TRUE)$value

  
  # ** feature data ---------------------------------------------------------------

  # ** ** side panel --------------------------------------------------------------
  source(file.path("server", "feature", "shift_cells.R"), local = TRUE)$value
  source(file.path("server", "feature", "split_pairs.R"), local = TRUE)$value
  source(file.path("server", "feature", "split_cols.R"), local = TRUE)$value
  source(file.path("server", "feature", "save_data.R"), local = TRUE)$value

  # ** ** main panel --------------------------------------------------------------
  source(file.path("server", "feature", "display_data.R"), local = TRUE)$value
  source(file.path("server", "feature", "graphical_summary.R"), local = TRUE)$value
  

  # ** all data -------------------------------------------------------------------
  
  # ** ** side panel --------------------------------------------------------------
  source(file.path("server", "all_data", "filter_rows.R"), local = TRUE)$value
  source(file.path("server", "all_data", "join_dfs.R"), local = TRUE)$value
  source(file.path("server", "all_data", "save_data.R"), local = TRUE)$value
  
  # ** ** main panel --------------------------------------------------------------
  source(file.path("server", "all_data", "workbench.R"), local = TRUE)$value
  source(file.path("server", "all_data", "data_viewer.R"), local = TRUE)$value
  
  
  # ** help modals -------------------------------------------------------------
  
  source(file.path("server", "help_modals.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
