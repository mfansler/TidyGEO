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
  library(shinydashboard)
  library(rmarkdown)
  source("tidygeo_functions.R")
  source("server/formatting_helper_functions.R")
})

#start_time <- Sys.time()
series_list <- read_feather("www/series_list.feather")
platform_list <- read_feather("www/platform_list.feather")
#end_time <- Sys.time()
#print(paste("Reading files", end_time - start_time))

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
               menuSubItem("Clinical data", icon = icon("clipboard"), tabName = "clinical_data"),
               menuSubItem("Assay data", icon = icon("microscope"), tabName = "assay_data"),
               menuSubItem("Feature data", icon = icon("dna"), tabName = "feature_data"),
               menuSubItem("All data", icon = icon("cubes"), tabName = "all_data")),
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
      tabItem("clinical_data",
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
      
      tabItem("assay_data",
               
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
      
      tabItem("feature_data",
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
      
      tabItem("all_data",
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
               h2(paste("Version:", version)),
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
  
  # For any of the following, if you change the variable names of 
  # "[]_data" or "last_data", get ready for a world of pain... er, refactoring.
  # 
  # Also, I like to keep these all in separate lists so that they can update independently
  # of each other. Otherwise, an update--to, for example, assay_data--might cause values from
  # clinical_vals to update, which would be ridiculously slow. Usually best to keep reactiveValues
  # as distinct and small as possible.
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
      #oFile = commentify(" "),
      #download_chunk_len = 0,
      #current_chunk_len = 0,
      plot_to_save = NULL,
      shift_results = list(),
      use_viewing_subset = FALSE,
      viewing_min = 1,
      viewing_subset = c(1, 5)
    )
  
  assay_vals <-
    reactiveValues(
      display_default = data.frame("Please load a dataset"),
      orig_data = NULL,
      assay_data = NULL,
      last_data = NULL,
      #oFile = commentify(" "),
      #download_chunk_len = 0,
      #current_chunk_len = 0,
      id_col = "ID",
      prev_id = "ID",
      plot_to_save = NULL,
      disable_btns = FALSE,
      expression_warning_state = FALSE,
      use_viewing_subset = FALSE,
      viewing_min = 2,
      viewing_subset = c(2, 6)
    )
  
  feature_vals <-
    reactiveValues(
      feature_data = NULL,
      shift_results = NULL,
      last_data = NULL,
      #oFile = commentify(" "),
      #current_chunk_len = 0,
      orig_data = NULL,
      ft_default = data.frame("Please load a dataset"),
      id_col = "ID",
      prev_id = "ID",
      plot_to_save = NULL,
      use_viewing_subset = FALSE,
      viewing_min = 2,
      viewing_subset = c(2, 6)
    )
  
  all_vals <- 
    reactiveValues(
      all_data = NULL,
      last_data = NULL,
      last_selected_match1 = NULL,
      last_selected_match2 = NULL,
      join_datatypes_visible = 1,
      use_viewing_subset = FALSE,
      viewing_min = 1,
      viewing_subset = c(1, 5)
    )
  
  
  # ** General functions for all datatypes -------------------------------------

  # ** ** For passing a dataset through a formatting function ---------------------
  # >> This function is not necessary if you know which datatype you're updating, but
  #    helpful since it writes the function to the script automatically.
  # 
  # >> func_to_eval must be a string
  # >> func_args must be a list; it can be named, for named arguments
  # >> Leave to_knit NULL in most cases, unless you're joining datasets or using values from one
  #    dataset to format another
  # 
  # >> This function must stay in the server function to have access to the reactiveValues
  # 
  # >> Please see https://adv-r.hadley.nz/quasiquotation.html (section 9.4) for details about '!!'
  eval_function <- function(datatype, func_to_eval, func_args, header = "", to_knit = NULL) {
    if (!datatype %in% allowed_datatypes) {
      stop(invalid_datatype_message)
    }
    
    # WRITING COMMANDS TO R SCRIPT
    set_undo_point_script(datatype)
    if (length(to_knit) == 2) {
      knit_scripts(to_knit[1], to_knit[2], datatype)
    } else if (length(to_knit) > 0) {
      stop(paste0("Please provide two datatypes to knit together. You have provided ", length(to_knit), "."))
    }
    save_lines(commentify(header), datatype, "body")
    add_function(func_to_eval, datatype)
    func_args_text <- lapply(func_args, function(arg) {
      if (class(arg) == "call") {
        sym(get_datatype_expr_text(arg))
      } else {
        arg
      }
    })
    save_lines(
      paste0(
        datatype, 
        "_data <- ", 
        rlang::expr_text(rlang::expr((!!func_to_eval)(!!sym(paste0(datatype, "_data")), !!!func_args_text)))
      ),
      datatype,
      "body"
    )
    
    status <- tryCatch({ # Whether the formatting function evaluated successfully
      # Get the result of the formatting function
      result <- eval(
      expr((!!func_to_eval)(
        eval(`$`(!!sym(paste0(datatype, "_vals")), !!paste0(datatype, "_data"))), 
        !!!func_args
            )
        )
      )
      "completed"
    }, error = function(e) {
      return(paste0("Error:\n", paste(e, collapse = "\n")))
    })
    
    if (status != "completed") { # The formatting was not performed
      undo_script(datatype)
      return(status)
    } else { # The formatting function was successful
      # Replace the last data with the current data (before formatting)
      eval(
        expr(`<-`(`$`(!!sym(paste0(datatype, "_vals")), "last_data"), `$`(!!sym(paste0(datatype, "_vals")), !!paste0(datatype, "_data"))))
      )
      
      # Replace the data with the formatted data
      eval(
        expr(`<-`(`$`(!!sym(paste0(datatype, "_vals")), !!paste0(datatype, "_data")), result))
      )
      return(status)
    }
  }
  
  # ** ** reset ----------------------------------------------------------------
  reset_datatype <- function(datatype) {
    if (length(datatype) == 1 && datatype %in% allowed_datatypes) {
      eval(
        expr(`<-`(`$`(!!sym(paste0(datatype, "_vals")), !!paste0(datatype, "_data")), `$`(!!sym(paste0(datatype, "_vals")), "orig_data")))
      )
      reset_script(datatype)
    } else {
      stop(paste("Error in undo_last_action.", invalid_datatype_message))
    }
  }
  
  # ** ** undo --------------------------------------------------------------------
  undo_last_action <- function(datatype) {
    if (length(datatype) == 1 && datatype %in% allowed_datatypes) {
      eval(
        expr(`<-`(`$`(!!sym(paste0(datatype, "_vals")), !!paste0(datatype, "_data")), `$`(!!sym(paste0(datatype, "_vals")), "last_data")))
      )
      undo_script(datatype)
    } else {
      stop(paste("Error in undo_last_action.", invalid_datatype_message))
    }
  }
  
  # To use these, make sure you have lines similar to the following wherever you are
  # displaying the data:
  # 
  # observe({
  #   assay_vals$use_viewing_subset <- !is.null(ncol(assay_vals$assay_data)) && ncol(assay_vals$assay_data) > 19
  #   assay_vals$viewing_subset <- c(2, min(6, ncol(assay_vals$assay_data)))
  # }, priority = 1)
  #
  # assay_in_view <- reactive({
  #   if (assay_vals$use_viewing_subset) {
  #     assay_vals$assay_data[c(1, assay_vals$viewing_subset[1]:assay_vals$viewing_subset[2])]
  #   } else {
  #     assay_vals$assay_data
  #   }
  # })
  # 
  # output$col_nav <- renderUI({
  #   if (assay_vals$use_viewing_subset) {
  #     col_navigation_set("assay")
  #   }
  # })
  
  observeEvent(input$next_cols_clicked, {
    to_move <- allowed_datatypes[which(str_detect(input$next_cols_clicked, allowed_datatypes))]
    if (identical(to_move, character(0))) {
      stop("Error in clicking next cols. The button that called next cols is not tagged with a valid datatype.")
    } else {
      ncol_data_to_move <- eval(expr(ncol(!!get_datatype_expr(to_move))))
      move_by <- floor(ncol_data_to_move / 5)
      current_subset <- eval(expr(`$`(!!sym(paste0(to_move, "_vals")), "viewing_subset")))[1]
      
      start <- min(ncol_data_to_move, current_subset + move_by)
      end <- min(ncol_data_to_move, start + move_by)
      
      eval(
        expr(`<-`(`$`(!!sym(paste0(to_move, "_vals")), "viewing_subset"), c(start, end)))
      )
    }
    session$sendCustomMessage("resetValue", "next_cols_clicked")
  })
  
  observeEvent(input$prev_cols_clicked, {
    to_move <- allowed_datatypes[which(str_detect(input$prev_cols_clicked, allowed_datatypes))]
    if (identical(to_move, character(0))) {
      stop("Error in clicking next cols. The button that called prev cols is not tagged with a valid datatype.")
    } else {
      ncol_data_to_move <- eval(expr(ncol(!!get_datatype_expr(to_move))))
      move_by <- floor(ncol_data_to_move / 5)
      viewing_min <- eval(expr(`$`(!!sym(paste0(to_move, "_vals")), "viewing_min")))
      current_subset <- eval(expr(`$`(!!sym(paste0(to_move, "_vals")), "viewing_subset")))[2]
      
      end <- max(viewing_min, current_subset - move_by)
      start <- max(viewing_min, end - move_by)
      
      eval(
        expr(`<-`(`$`(!!sym(paste0(to_move, "_vals")), "viewing_subset"), c(start, end)))
      )
    }
    session$sendCustomMessage("resetValue", "prev_cols_clicked")
  })

  
  # ** Choose dataset ----------------------------------------------------------

  source(file.path("server", "clinical", "choose_dataset.R"), local = TRUE)$value

  
  # ** Data getters ------------------------------------------------------------

  get_clinical_data <- function() {
    clinical_vals$clinical_data <- withProgress(process_clinical(values$allData, session))
    clinical_vals$last_data <- clinical_vals$clinical_data
    clinical_vals$orig_data <- clinical_vals$clinical_data
    
    #WRITING COMMANDS TO R SCRIPT
    save_lines(commentify("extract clinical data"), "clinical", "body")
    add_function("process_clinical", "clinical")
    save_lines("clinical_data <- process_clinical(series_data)", "clinical", "body")
    set_reset_point_script("clinical")
    
  }
  
  get_assay_data <- function() {
    closeAlert(session, "nonnumeric")
    
    extracted_data <- withProgress(process_expression(values$allData, session = session), message = "Processing assay data")
    
    assay_vals$warning_state <- extracted_data[["status"]]
    
    assay_vals$orig_data <- extracted_data[["expressionData"]]
    
    if (is.null(assay_vals$orig_data)) {
      assay_vals$display_default <- data.frame(paste0("No assay data available for ", input$geoID))
    } else {
      assay_vals$id_col <- colnames(assay_vals$orig_data)[1]
      assay_vals$prev_id <- assay_vals$id_col
      
      assay_vals$last_data <- assay_vals$orig_data
      assay_vals$assay_data <- assay_vals$orig_data
      
      assay_vals$viewing_subset <- c(2, min(6, ncol(assay_vals$assay_data)))
      
      save_lines(commentify("extract expression data"), "assay", "body")
      add_function("process_expression", "assay")
      save_lines(c("extracted_data <- process_expression(series_data)",
                                      "assay_data <- extracted_data[['expressionData']]"), 
                                    "assay", "body")
      set_reset_point_script("assay")
    }
    rm(extracted_data)
    shinyjs::toggleState("undoEvalExpr", FALSE)
  }
  
  get_feature_data <- function() {
    withProgress({
      feature_vals$orig_data <- process_feature(values$allData, session = session)
      }, message = "Processing feature data")
    if (is.null(feature_vals$orig_data)) {
      feature_vals$ft_default <- data.frame(paste0("No feature data available for ", input$geoID))
    } else {
      feature_vals$id_col <- colnames(feature_vals$orig_data)[1]
      feature_vals$prev_id <- feature_vals$id_col
      
      #feature_vals$orig_data <- find_intersection(feature_vals$orig_data, assay_vals$assay_data)
      feature_vals$last_data <- feature_vals$orig_data
      feature_vals$feature_data <- feature_vals$orig_data
      
      feature_vals$viewing_subset <- c(2, min(6, ncol(feature_vals$feature_data)))
      
      save_lines(commentify("extract feature data"), "feature", "body")
      add_function("process_feature", "feature")
      save_lines(c("feature_data <- process_feature(series_data)"), 
                                      "feature", "body")
      set_reset_point_script("feature")
    }
  }
  
  observeEvent(input$top_level, {
    if (!is.null(values$allData)) {
      if (input$top_level == "clinical_data" && is.null(clinical_vals$clinical_data)) {
        get_clinical_data()
      } else if (input$top_level == "assay_data" && is.null(assay_vals$assay_data)) {
        get_assay_data()
      } else if (input$top_level == "feature_data" && is.null(feature_vals$feature_data)) {
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

  
  
  
  # ** navigation --------------------------------------------------------------
  
  source(file.path("server", "navigation.R"), local = TRUE)$value
  
  
  # ** help modals -------------------------------------------------------------
  
  source(file.path("server", "help_modals.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
