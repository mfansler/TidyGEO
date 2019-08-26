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
  
  if (is.null(get_data_member("assay", "orig_data"))) {
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

#' Get the expression to set a variable (which may be a reactive variable
#' from one of the four lists: clinical_vals, assay_vals, feature_vals, or all_vals)
#' equal to some object.
#' 
#' @param x The variable to set equal to the object.
#' @param y The object to be stored in the variable.
#' @param x_datatype If x is a reactive variable from one of the four lists
#' (clinical_vals, assay_vals, feature_vals, or all_vals), then this must equal
#' the string name of the list ("clinical", "assay", "feature", or "all).
#' Otherwise, this must remain NULL.
#' @return The expression to set a variable equal to some object.
#' @examples
#' # The following line is equivalent to `expr(clinical_vals$clinical_data <- data.frame())`:
#' get_x_equalto_y_expr("clinical_data", data.frame(), "clinical")
#' # The following lines are equivalent to `expr(result <- clinical_vals$clinical_data)`
#' get_x_equalto_y_expr(result, clinical_vals$clinical_data)
#' get_x_equalto_y_expr(result, get_data_member("clinical", "clinical_data"))
get_x_equalto_y_expr <- function(x, y, x_datatype = NULL) {
  x_name <- if (is.null(x_datatype)) x else get_data_member_expr(x_datatype, x)
  expr(`<-`(!!x_name, !!y))
}

#' Set a variable (which may be a reactive variable from one of the four lists: 
#' clinical_vals, assay_vals, feature_vals, or all_vals) equal to some object.
#' 
#' @param x The variable to set equal to the object.
#' @param y The object to be stored in the variable.
#' @param x_datatype If x is a reactive variable from one of the four lists
#' (clinical_vals, assay_vals, feature_vals, or all_vals), then this must equal
#' the string name of the list ("clinical", "assay", "feature", or "all).
#' Otherwise, this must remain NULL.
#' @return The expression to set a variable equal to some object.
#' @examples
#' # The following line is equivalent to `clinical_vals$clinical_data <- data.frame()`:
#' set_x_equalto_y("clinical_data", data.frame(), "clinical")
#' # The following lines are equivalent to `result <- clinical_vals$clinical_data`
#' set_x_equalto_y(result, clinical_vals$clinical_data)
#' set_x_equalto_y(result, get_data_member("clinical", "clinical_data"))
set_x_equalto_y <- function(x, y, x_datatype = NULL) {
  eval(get_x_equalto_y_expr(x, y, x_datatype))
}

#' Get the name associated with the "data" variable from one of the four lists:
#' clinical_vals, assay_vals, feature_vals, or all_vals.
#' 
#' @param datatype_expr An expression object corresponding to the "data" variable
#' from one of the four lists, i.e., clinical_vals$clinical_data, assay_vals$assay_data,
#' feature_vals$feature_data, all_vals$all_data
#' @return A string corresponding to the "data" variable from one of the four reactive lists,
#' i.e., "clinical", "assay", "feature", or "all"
#' @examples 
#' get_datatype_expr_text(expr(clinical_vals$clinical_data))
get_datatype_expr_text <- function(datatype_expr) {
  if (class(datatype_expr) != "call") {
    stop("Error in get_datatype_expr_text. 'datatype_expr' is not an expression.")
  }
  expr_text <- rlang::expr_text(datatype_expr)
  if (grepl("\\$", expr_text)) {
    str_remove(str_split(expr_text, pattern = "\\$")[[1]][2], "\\)")
  } else {
    stop("Error in get_datatype_expr_text. 'datatype_expr' does not represent one of the datatype reactiveVariables.")
  }
}

data_loaded <- function(datatype) {
  !is.null(eval(get_data_member_expr(datatype, dataname(datatype))))
}

error_modal <- function(title, subtitle, error_message) {
  modalDialog(
    title = HTML(paste0('<font color="red">', title, '</font>')),
    p(paste(subtitle, "Reason:"), style = "color:red"),
    p(error_message, style = "color:red")
  )
}

#' Get the expression to get a reactive variable from one of the four lists: 
#' clinical_vals, assay_vals, feature_vals, or all_vals.
#'
#' @param datatype The name of the list to grab from ("clinical", "assay", "feature", "all").
#' @param member_name The name of the reactive variable within the list.
#' @return An expression to get a variable.
#' @examples
#' # To get the expression to get the variable called "clinical_data" from the "clinical_vals" list
#' get_data_member_expr("clinical", "clinical_data")
get_data_member_expr <- function(datatype, member_name) {
  expr(`$`(!!sym(varname(datatype)), !!member_name))
}

#' Get a reactive variable from one of the four lists: 
#' clinical_vals, assay_vals, feature_vals, or all_vals.
#' 
#' @param datatype The name of the list to grab from (clinical, assay, feature, all)
#' @param member_name The name of the reactive variable within the list.
#' @return An object from the list specified by datatype.
#' @examples 
#' # To get the variable called "clinical_data" from the "clinical_vals" list
#' get_data_member("clinical", "clinical_data")
get_data_member <- function(datatype, member_name) {
  eval(get_data_member_expr(datatype, member_name))
}

#' Gets the input associated with an ID
#' 
#' @param id A string indicating the ID for the input
#' @return The object stored in that input
#' 
#' @examples 
#' my_id <- "My_ID"
#' `ui <- fluidPage(actionButton(my_id, "Click me!"))
#' server <- function(input, output, session) {
#'   observeEvent(get_input(my_id), {
#'     print("clicked")
#'   })
#' }
#' runApp(shinyApp(ui, server))
get_input <- function(id) {
  eval(parse(text = paste0("input$", id)))
}

col_navigation_set_server <- function(datatype, extra_tag = NULL) {
  observe({
    n_cols <- ncol(get_data_member(datatype, dataname(datatype)))
    this_viewing_min <- get_data_member(datatype, "viewing_min")
    set_x_equalto_y("use_viewing_subset", !is.null(n_cols) && n_cols > 19, datatype)
    set_x_equalto_y("viewing_subset", c(this_viewing_min, min(this_viewing_min + 5, n_cols)), datatype)
  }, priority = 1)
  
  assign(view(datatype), reactive({
    if (get_data_member(datatype, "use_viewing_subset")) {
      this_viewing_subset <- get_data_member(datatype, "viewing_subset")
      cols_to_view <- this_viewing_subset[1]:this_viewing_subset[2]
      if (get_data_member(datatype, "viewing_min") == 2) {
        cols_to_view <- c(1, cols_to_view)
      }
      get_data_member(datatype, dataname(datatype))[cols_to_view]
    } else {
      get_data_member(datatype, dataname(datatype))
    }
  }), pos = parent.frame())
  
  output[[visible(datatype, extra_tag)]] <- renderText({
    paste("Showing", 
          get_data_member(datatype, "viewing_subset")[1], "to", 
          get_data_member(datatype, "viewing_subset")[2], "of", 
          ncol(get_data_member(datatype, dataname(datatype))), 
          "columns")
  })
}

table_for_col_navigation_server <- function(datatype, extra_tag = NULL, show_rownames = FALSE, show_filters = FALSE) {
  filter <- if (show_filters) list(position = "top", clear = FALSE) else c("none")
  
  output[[display(datatype, extra_tag)]] <- DT::renderDT({
    if (!is.null(get_data_member(datatype, dataname(datatype)))) {
      datatable(do.call(view(datatype), list()), filter = filter, rownames = show_rownames, 
                options = c(BASIC_TABLE_OPTIONS, pageLength = get_data_member(datatype, "user_pagelen")))
    }
    else {
      empty_table(get_data_member(datatype, "display_default"))
    }
  })
  
  if (show_filters) {
    output[[paste(datatype, extra_tag, "evaluate", "filters", "button", sep = SEP)]] <- renderUI({
      if (!is.null(get_input(paste(display(datatype, extra_tag), "search", "columns", sep = "_"))) && !all(get_input(paste(display(datatype, extra_tag), "search", "columns", sep = "_")) == "")) {
        div(
          primary_button(paste(datatype, extra_tag, "evaluate", "filters", sep = SEP), 
                         label = div(icon("filter"),
                                     "Apply filters"),
                         width = '200px', class = "indent"),
          help_link(paste(datatype, extra_tag, sep = SEP), "evaluate_filters_help"))
      }
    })
    
    observeEvent(get_input(paste(datatype, extra_tag, "evaluate", "filters", sep = SEP)), {
      to_filter <- get_input(paste(display(datatype, extra_tag), "search", "columns", sep = "_"))
      names(to_filter) <- colnames(get_data_member(datatype, dataname(datatype)))[get_data_member(datatype, "viewing_subset")[1]:get_data_member(datatype, "viewing_subset")[2]]
      status <- eval_function(datatype, "filterExpressionData", list(to_filter), "filter data")
      if (status != SUCCESS) {
        showModal(
          error_modal(paste("Error in filter", datatype, "data"), "Filters not saved.", status)
        )
      }
      if (datatype == "assay") {
        shinyjs::toggleState("undoEvalExpr", TRUE)
      }
    })
  }
}

graphical_summary_server <- function(datatype, extra_tag = NULL) {
  data_expr <- expr(do.call(view(datatype), list()))
  output[[paste("choose", var_to_view(datatype, extra_tag), sep = SEP)]] <- renderUI({
    if (data_loaded(datatype)) {
      choices <- 1:length(colnames(eval(data_expr)))
      choice_names <- colnames(eval(data_expr))
      choices <- c(0, choices)
      choice_names <- c("(view all)", choice_names)
      names(choices) <- choice_names
      
      selectInput(var_to_view(datatype, extra_tag), label = "Choose a variable to view:", choices = choices)
    }
  })
  
  # Create divs
  output[[paste(datatype, extra_tag, "plots", sep = SEP)]] <- renderUI({
    
    if (data_loaded(datatype)) {
      plot_output_list <- lapply(1:ncol(eval(data_expr)), function(i) {
        if (is_all_identical(eval(data_expr)[,i])) {
          div(hr(), HTML(
            paste0("<b>", colnames(eval(data_expr))[i], "</b> consists of all the same value.")
          ), hr())
        } else if (is_all_unique(eval(data_expr)[,i]) && !isAllNum(eval(data_expr)[i])) {
          div(hr(), HTML(
            paste0("<b>", colnames(eval(data_expr))[i], "</b> consists of all unique values.")
          ), hr())
        } else {
          plotname <- make.names(colnames(eval(data_expr))[i])
          div(
            withSpinner(plotlyOutput(plotname, height = 700, width = "auto"), type = SPINNER_TYPE), 
            tertiary_button(paste(datatype, extra_tag, "savePlot", i, sep = SEP), div(SAVE_ICON, "Download plot"), class = paste(datatype, extra_tag, "plot", sep = SEP))
          )
        }
      })
      if (!is.null(get_input(var_to_view(datatype, extra_tag)))) {
        if (as.numeric(get_input(var_to_view(datatype, extra_tag))) == 0) {
          do.call(tagList, plot_output_list)
        } else {
          plot_output_list[as.numeric(get_input(var_to_view(datatype, extra_tag)))][[1]]
        }
      }
    }
  })
  # Create the actual plots associated with the plot names
  observe({
    if (!is.null(eval(data_expr))) {
      lapply(1:ncol(eval(data_expr)), function(i){
        if (!is_all_identical(eval(data_expr)[,i])) {
          is_all_numeric <- isAllNum(eval(data_expr)[i])
          if (!is_all_unique(eval(data_expr)[,i]) || is_all_numeric) {
            output[[ make.names(colnames(eval(data_expr))[i]) ]] <- renderPlotly({
              suppressWarnings(create_plot(
                as.character(eval(data_expr)[,i]), 
                get_input(paste(datatype, extra_tag, "plot", "color", sep = SEP)), 
                get_input(paste(datatype, extra_tag, "binwidths", sep = SEP)), 
                colnames(eval(data_expr))[i], 
                is_all_numeric
              ))
            })
          }
        }
      })
    }
  })
  
  observeEvent(get_input(paste("last", "btn", datatype, extra_tag, sep = SEP)), {
    if (!is.null(get_input(paste("last", "btn", datatype, extra_tag, sep = SEP)))) {
      set_x_equalto_y("plot_to_save", 
                      as.numeric(as.character(str_remove(
                        get_input(paste("last", "btn", datatype, extra_tag, sep = SEP)), 
                        paste0(datatype, SEP, extra_tag, SEP, "savePlot", SEP))
                      )), 
                      datatype)
      showModal(
        modalDialog(
          sliderInput(paste(datatype, extra_tag, "plot", "width", sep = SEP), label = paste0("Image width (", PLOT_UNITS, "):"), min = 20, max = 100, value = 60),
          sliderInput(paste(datatype, extra_tag, "plot", "height", sep = SEP), label = paste0("Image height (", PLOT_UNITS, "):"), min = 20, max = 100, value = 60),
          radioButtons(paste(datatype, extra_tag, "plot", "filetype", sep = SEP), label = "File type:", choices = c("PDF" = "pdf", "JPG" = "jpg", "PNG" = "png")),
          downloadButton(paste(datatype, extra_tag, "plot", "download", sep = SEP)),
          footer = modalButton("Close")
        ))
      session$sendCustomMessage(type = "resetValue", paste("last", "btn", datatype, extra_tag, sep = SEP))
    }
  })
  
  output[[paste(datatype, extra_tag, "plot", "download", sep = SEP)]] <- downloadHandler(
    filename = function() {
      paste(
        make.names(colnames(eval(data_expr))[get_data_member(datatype, "plot_to_save")]), 
        get_input(paste(datatype, extra_tag, "plot", "filetype", sep = SEP)), 
        sep = "."
      )
    },
    content = function(file) {
      plot_to_save <- create_plot_to_save(
        eval(data_expr)[,get_data_member(datatype, "plot_to_save")], 
        get_input(paste(datatype, extra_tag, "plot", "color", sep = SEP)), 
        get_input(paste(datatype, extra_tag, "binwidths", sep = SEP)), 
        colnames(eval(data_expr))[get_data_member(datatype, "plot_to_save")], 
        isAllNum(eval(data_expr)[get_data_member(datatype, "plot_to_save")]))
      
      ggsave(
        file, 
        plot_to_save, 
        width = get_input(paste(datatype, extra_tag, "plot", "width", sep = SEP)), 
        height = get_input(paste(datatype, extra_tag, "plot", "height", sep = SEP)), 
        units = PLOT_UNITS, 
        device = get_input(paste(datatype, extra_tag, "plot", "filetype", sep = SEP))
      )
      
    }
  )
}
# creates observers for a navigation set
navigation_set_server <- function(prev, from, to, section_prev = NULL, section_to = NULL) {
  observeEvent(get_input(nav(from, prev, section_prev)), {
    updateTabsetPanel(session, section_prev, selected = prev)
  })
  observeEvent(get_input(nav(from, to, section_to)), {
    updateTabsetPanel(session, section_to, selected = to)
  })
}