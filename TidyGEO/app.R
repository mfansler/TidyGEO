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
  library(rlang)
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
               menuSubItem("All data", icon = , tabName = "all_data")),
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
  
  # This is my roundabout version of a class structure--the class is called DataType. 
  # (Object-oriented programming in R is possible, but difficult with reactiveValues 
  # because the creators of shiny did not make it easy to inherit from the ReactiveValues class.)
  DataType <- function(datatype, ...) {
    eval(expr(reactiveValues(
      display_default = data.frame("Please load a dataset."),
      !!!list2(!!dataname(datatype) := NULL),
      orig_data = NULL,
      last_data = NULL,
      plot_to_save = NULL,
      use_viewing_subset = FALSE,
      viewing_min = 1,
      viewing_subset = c(1, MOVE_BY),
      user_pagelen = DEFAULT_PAGELEN,
      !!!list(...)
    )))
  }

  
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
  
  # The following variables, if changed, will cause need for extensive refactoring:
  # display_default
  # []_data
  # orig_data
  # last_data
  # use_viewing_subset
  # viewing_min
  # viewing_subset
  # user_pagelen
  # 
  # Most refactoring will probably take place in tidygeo_functions.R.
  # 
  # Reactive values reside here in separate lists so that they can update independently
  # of each other. Otherwise, an update--to, for example, assay_data--might cause values from
  # clinical_vals to update, which would be ridiculously slow. Usually best to keep reactiveValues
  # as distinct and small as possible.
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
  # clinical_vals <- 
  #  reactiveValues(
  #    display_default = data.frame("Please load a dataset."),
  #    clinical_data = NULL,
  #    orig_data = NULL,
  #    last_data = NULL,
  #    last_selected_rename = NULL,
  #    last_selected_substitute = NULL,
  #    last_selected_exclude = NULL,
  #    subs_input = data.frame(),
  #    subs_display = data.frame(
  #      To_Replace = "",
  #      New_Val = "",
  #      stringsAsFactors = FALSE
  #    ),
  #    #oFile = commentify(" "),
  #    #download_chunk_len = 0,
  #    #current_chunk_len = 0,
  #    plot_to_save = NULL,
  #    shift_results = list(),
  #    use_viewing_subset = FALSE,
  #    viewing_min = 1,
  #    viewing_subset = c(1, 5),
  #    user_pagelen = 10
  #  )
  
  assign(
    varname("assay"),
    DataType("assay",
             id_col = "ID",
             prev_id = "ID",
             disable_btns = FALSE,
             expression_warning_state = FALSE
    )
  )
  # assay_vals <- 
  #  reactiveValues(
  #    display_default = data.frame("Please load a dataset."),
  #    orig_data = NULL,
  #    assay_data = NULL,
  #    last_data = NULL,
  #    #oFile = commentify(" "),
  #    #download_chunk_len = 0,
  #    #current_chunk_len = 0,
  #    id_col = "ID",
  #    prev_id = "ID",
  #    plot_to_save = NULL,
  #    disable_btns = FALSE,
  #    expression_warning_state = FALSE,
  #    use_viewing_subset = FALSE,
  #    viewing_min = 2,
  #    viewing_subset = c(2, 6),
  #    user_pagelen = 10
  #  )
  assign(
    varname("feature"),
    DataType("feature",
             shift_results = NULL,
             id_col = "ID",
             prev_id = "ID"
    )
  )
  # feature_vals <-
  #  reactiveValues(
  #    feature_data = NULL,
  #    shift_results = NULL,
  #    last_data = NULL,
  #    #oFile = commentify(" "),
  #    #current_chunk_len = 0,
  #    orig_data = NULL,
  #    display_default = data.frame("Please load a dataset."),
  #    id_col = "ID",
  #    prev_id = "ID",
  #    plot_to_save = NULL,
  #    use_viewing_subset = FALSE,
  #    viewing_min = 2,
  #    viewing_subset = c(2, 6),
  #    user_pagelen = 10
  #  )
  
  assign(
    varname("all"),
    DataType("all",
             last_selected_match1 = NULL,
             last_selected_match2 = NULL,
             join_datatypes_visible = 1
    )
  )
  # all_vals <- 
  #  reactiveValues(
  #    display_default = data.frame("Please load a dataset."),
  #    all_data = NULL,
  #    last_data = NULL,
  #    last_selected_match1 = NULL,
  #    last_selected_match2 = NULL,
  #    join_datatypes_visible = 1,
  #    use_viewing_subset = FALSE,
  #    viewing_min = 1,
  #    viewing_subset = c(1, 5),
  #    user_pagelen = 10
  #  )
  
  
  # ** General functions for all datatypes -------------------------------------

  # ** ** evaluate a function --------------------------------------------------
  
  #' Perform some function on the "data" variable from one of the four lists (clinical, assay, feature, all)
  #' 
  #' This function is not necessary if you know which datatype you're updating, but
  #' helpful since it writes the function to the script automatically.
  #' 
  #' This function must stay in the server function to have access to the reactiveValues.
  #' 
  #' Please see https://adv-r.hadley.nz/quasiquotation.html (section 9.4) for details about '!!'
  #' 
  #' @param datatype A string corresponding to one of the datatypes ("clinical", "assay", "feature", or "all")
  #' @param func_to_eval A string corresponding to the name of a function in formatting_helper_functions.R.
  #' @param func_args A (named) list corresponding to the arguments to the function (after the "data" variable)
  #' @param header Text to use as the header for this section of the R script.
  #' @param to_knit If the function involves two different "data" variables, a list of the string names 
  #' (e.g. "clinical", "assay") of those variables so we know which scripts to knit together.
  #' @examples 
  #' # The following sections of code are equivalent.
  #' # Section 1:
  #' set_undo_point_script("clinical")
  #' commentify("filter columns", "clinical", "body")
  #' clinical_vals$clinical_data <- filterUninformativeColumns(clinical_vals$clinical_data, c("same_vals"))
  #' write_to_script("clinical_data <- filterUninformativeColumns(clinical_data, c('same_vals')))
  #' # Section 2:
  #' eval_function("clinical", "filterUninformativeColumns", list(c("same_vals")), "filter columns")
  eval_function <- function(datatype, func_to_eval, func_args, header = "", to_knit = NULL) {
    if (!datatype %in% ALLOWED_DATATYPES) {
      stop(INVALID_DATATYPE_MESSAGE)
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
      paste0(dataname(datatype),
        " <- ", 
        rlang::expr_text(rlang::expr((!!func_to_eval)(!!sym(dataname(datatype)), !!!func_args_text)))
      ),
      datatype,
      "body"
    )
    status <- tryCatch({ # Whether the formatting function evaluated successfully
      # Get the result of the formatting function
      result <- eval(
        #expr((!!func_to_eval)(
        #  eval(`$`(!!sym(paste0(datatype, "_vals")), !!paste0(datatype, "_data"))), 
        #  !!!func_args
        #      )
        #  )
        #)
        expr((!!func_to_eval)(
          get_data_member(datatype, dataname(datatype)), 
          !!!func_args
        )
        )
      )
      SUCCESS
    }, error = function(e) {
      return(paste0("Error:\n", paste(e, collapse = "\n")))
    })
    
    if (status != SUCCESS) { # The formatting was not performed
      undo_script(datatype)
      return(status)
    } else { # The formatting function was successful
      # Replace the last data with the current data (before formatting)
      #eval(
      #  expr(`<-`(`$`(!!sym(paste0(datatype, "_vals")), "last_data"), !!get_datatype_expr(datatype)))
      #  #expr(`<-`(`$`(!!sym(paste0(datatype, "_vals")), "last_data"), `$`(!!sym(paste0(datatype, "_vals")), !!paste0(datatype, "_data"))))
      #)
      set_x_equalto_y("last_data", get_data_member(datatype, dataname(datatype)), datatype)
      
      # Replace the data with the formatted data
      #eval(
      #  expr(`<-`(!!get_datatype_expr(datatype), result))
      #  #expr(`<-`(`$`(!!sym(paste0(datatype, "_vals")), !!paste0(datatype, "_data")), result))
      #)
      set_x_equalto_y(dataname(datatype), result, datatype)
      
      return(status)
    }
  }
  
  # ** ** reset ----------------------------------------------------------------
  reset_datatype <- function(datatype) {
    if (length(datatype) == 1 && datatype %in% ALLOWED_DATATYPES) {
      set_x_equalto_y(dataname(datatype), get_data_member(datatype, "orig_data"), datatype)
      reset_script(datatype)
    } else {
      stop(paste("Error in undo_last_action.", INVALID_DATATYPE_MESSAGE))
    }
  }
  
  # ** ** undo --------------------------------------------------------------------
  undo_last_action <- function(datatype) {
    if (length(datatype) == 1 && datatype %in% ALLOWED_DATATYPES) {
      set_x_equalto_y(dataname(datatype), get_data_member(datatype, "last_data"), datatype)
      undo_script(datatype)
    } else {
      stop(paste("Error in undo_last_action.", INVALID_DATATYPE_MESSAGE))
    }
  }

  # ** navigation --------------------------------------------------------------
  
  source(file.path("server", "navigation.R"), local = TRUE)$value
  
  
  # ** Choose dataset ----------------------------------------------------------

  source(file.path("server", "clinical", "choose_dataset.R"), local = TRUE)$value

  
  # ** Data getters ------------------------------------------------------------

  get_clinical_data <- function() {
    withProgress({
      set_x_equalto_y(dataname("clinical"), process_clinical(values$allData, session), "clinical")
    })
    set_x_equalto_y("last_data", get_data_member("clinical", dataname("clinical")), "clinical")
    set_x_equalto_y("orig_data", get_data_member("clinical", dataname("clinical")), "clinical")
    
    #WRITING COMMANDS TO R SCRIPT
    save_lines(commentify("extract clinical data"), "clinical", "body")
    add_function("process_clinical", "clinical")
    save_lines(paste0(dataname("clinical"), " <- process_clinical(", SERIES, ")"), "clinical", "body")
    set_reset_point_script("clinical")
    
  }
  
  get_assay_data <- function() {
    closeAlert(session, "nonnumeric")
    
    extracted_data <- withProgress(process_expression(values$allData, session = session), message = "Processing assay data")
    
    set_x_equalto_y("warning_state", extracted_data[["status"]], "assay")
    set_x_equalto_y("orig_data", extracted_data[["expressionData"]], "assay")
    
    if (is.null(get_data_member("assay", dataname("assay")))) {
      set_x_equalto_y("display_default", data.frame(paste0("No assay data available for ", input$geoID)), "assay")
    } else {
      set_x_equalto_y("id_col", colnames(get_data_member("assay", "orig_data"))[1], "assay")
      set_x_equalto_y("prev_id", get_data_member("assay", "id_col"), "assay")
      
      set_x_equalto_y("last_data", get_data_member("assay", "orig_data"), "assay")
      set_x_equalto_y(dataname("assay"), get_data_member("assay", "orig_data"), "assay")
      
      set_x_equalto_y("viewing_min", 2, "assay")
      set_x_equalto_y("viewing_subset", 
                      c(get_data_member("assay", "viewing_min"), 
                        min(MOVE_BY + 1, ncol(get_data_member("assay", dataname("assay"))))
                        ), 
                      "assay")
      
      save_lines(commentify("extract expression data"), "assay", "body")
      add_function("process_expression", "assay")
      save_lines(c(paste0("extracted_data <- process_expression(", SERIES, ")"),
                                      paste0(dataname("assay"), " <- extracted_data[['expressionData']]")), 
                                    "assay", "body")
      set_reset_point_script("assay")
    }
    rm(extracted_data)
    shinyjs::toggleState("undoEvalExpr", FALSE)
  }
  
  get_feature_data <- function() {
    withProgress({
      set_x_equalto_y("orig_data", process_feature(values$allData, session = session), "feature")
      }, message = "Processing feature data")
    if (is.null(get_data_member("feature", "orig_data"))) {
      set_x_equalto_y("display_default", data.frame(paste0("No feature data available for ", input$geoID)), "feature")
    } else {
      set_x_equalto_y("id_col", colnames(get_data_member("feature", "orig_data"))[1], "feature")
      set_x_equalto_y("prev_id", get_data_member("feature", "id_col"), "feature")
      
      #feature_vals$orig_data <- find_intersection(feature_vals$orig_data, assay_vals$assay_data)
      set_x_equalto_y("last_data", get_data_member("feature", "orig_data"), "feature")
      set_x_equalto_y(dataname("feature"), get_data_member("feature", "orig_data"), "feature")
      
      set_x_equalto_y("viewing_min", 2, "feature")
      set_x_equalto_y("viewing_subset", 
                      c(get_data_member("feature", "viewing_min"), 
                        min(MOVE_BY + 1, ncol(get_data_member("feature", dataname("feature"))))
                        ), 
                      "feature")
      
      save_lines(commentify("extract feature data"), "feature", "body")
      add_function("process_feature", "feature")
      save_lines(c(paste0("feature_data <- process_feature(", SERIES, ")")), 
                                      "feature", "body")
      set_reset_point_script("feature")
    }
  }
  
  observeEvent(input$top_level, {
    if (!is.null(values$allData)) {
      if (top_level != dataname("all") && is.null(get_data_member(top_level, dataname(top_level)))) {
        do.call(paste("get", top_level, "data", sep = "_"), list())
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
