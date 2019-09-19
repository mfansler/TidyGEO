source("tidygeo_variables.R")
source("tidygeo_functions.R")
source("ui/tidygeo_ui_functions.R")
source("server/formatting_helper_functions.R")

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
               source(file.path("ui", "choose_dataset", "choose_dataset.R"), local = TRUE)$value
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
  
  source(file.path("server", "tidygeo_server_functions.R"), local = TRUE)$value

  
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
  
  for (dt in ALLOWED_DATATYPES) {
    set_up_col_navigation(dt)
  }
  
  observeEvent(input$next_cols_clicked, {
    to_move <- ALLOWED_DATATYPES[which(str_detect(input$next_cols_clicked, ALLOWED_DATATYPES))]
    if (identical(to_move, character(0))) {
      stop("Error in clicking next cols. The button that called next cols is not tagged with a valid datatype.")
    } else {
      ncol_data_to_move <- ncol(get_data_member(to_move, dataname(to_move)))
      #move_by <- floor(ncol_data_to_move / 5)
      current_subset <- get_data_member(to_move, "viewing_subset")[1]
      
      start <- min(ncol_data_to_move, current_subset + MOVE_BY + 1)
      end <- min(ncol_data_to_move, start + MOVE_BY)
      
      set_x_equalto_y(
        "user_pagelen",
        input[[paste0(display(next_col_source(input$next_cols_clicked)), "_state")]][["length"]],
        to_move
        )
      set_x_equalto_y(
        "user_startrow",
        input[[paste0(display(next_col_source(input$next_cols_clicked)), "_state")]][["start"]],
        to_move
      )
      #eval(
      #  expr(
      #    `<-`(
      #      !!get_data_member_expr(to_move, "user_pagelen"),
      #      input[[paste0(display(next_col_source(input$next_cols_clicked)), "_state")]][["length"]]
      #    )
      #  )
      #)
      
      set_x_equalto_y(
        "viewing_subset",
        c(start, end),
        to_move
      )
      #eval(
      #  expr(`<-`(!!get_data_member_expr(to_move, "viewing_subset"), c(start, end)))
      #)
    }
    session$sendCustomMessage("resetValue", "next_cols_clicked")
  })
  
  observeEvent(input$prev_cols_clicked, {
    to_move <- ALLOWED_DATATYPES[which(str_detect(input$prev_cols_clicked, ALLOWED_DATATYPES))]
    if (identical(to_move, character(0))) {
      stop("Error in clicking next cols. The button that called prev cols is not tagged with a valid datatype.")
    } else {
      ncol_data_to_move <- ncol(get_data_member(to_move, dataname(to_move)))
      #move_by <- floor(ncol_data_to_move / 5)
      viewing_min <- get_data_member(to_move, "viewing_min")
      current_subset <- get_data_member(to_move, "viewing_subset")[2]
      
      end <- max(viewing_min, current_subset - MOVE_BY - 1)
      start <- max(viewing_min, end - MOVE_BY)
      
      set_x_equalto_y(
        "user_pagelen", 
        input[[paste0(display(prev_col_source(input$prev_cols_clicked)), "_state")]][["length"]],
        to_move)
      set_x_equalto_y(
        "user_startrow",
        input[[paste0(display(prev_col_source(input$prev_cols_clicked)), "_state")]][["start"]],
        to_move
      )
      #eval(
      #  expr(
      #    `<-`(
      #      !!get_data_member_expr(to_move, "user_pagelen"),
      #      input[[paste0(display(prev_col_source(input$prev_cols_clicked)), "_state")]][["length"]]
      #    )
      #  )
      #)
      
      set_x_equalto_y(
        "viewing_subset",
        c(start, end),
        to_move
      )
      #eval(
      #  expr(`<-`(!!get_data_member_expr(to_move, "viewing_subset"), c(start, end)))
      #)
    }
    session$sendCustomMessage("resetValue", "prev_cols_clicked")
  })
  
  # ** Choose dataset ----------------------------------------------------------

  source(file.path("server", "choose_dataset", "choose_dataset.R"), local = TRUE)$value

  
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
