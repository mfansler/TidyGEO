
# Small utility functions -------------------------------------------------

#' Capitalizes the first letter of a string.
#' 
#' @param my_str A string.
#' @return A string with the first letter capitalized.
#' 
#' @examples 
#' cap_first("carrots")
cap_first <- function(my_str) {
  paste0(toupper(substring(my_str, 1, 1)), substring(my_str, 2, nchar(my_str)))
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
  env <- parent.frame()
  eval(parse(text = paste0("input$", id)), envir = env)
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
  envir <- parent.frame()
  #eval(get_data_member_expr(datatype, member_name), envir)
  eval(get_data_member_expr(datatype, member_name), envir = envir)
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
  envir <- parent.frame()
  eval(get_x_equalto_y_expr(x, y, x_datatype), envir)
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
  envir <- parent.frame()
  !is.null(eval(get_data_member_expr(datatype, dataname(datatype)), envir = envir))
}

error_modal <- function(title, subtitle, error_message) {
  modalDialog(
    title = HTML(paste0('<font color="red">', title, '</font>')),
    p(paste(subtitle, "Reason:"), style = "color:red"),
    p(error_message, style = "color:red")
  )
}

# R Script writing --------------------------------------------------------


# ** initialize scripts ---------------------------------------------------

header <- c()

script_template <- list(
  libraries = c(),
  functions = c(),
  body = c(),
  end = c()
)

original_scripts <- list(
  clinical = script_template,
  assay = script_template,
  feature = script_template,
  all = script_template
)

last_scripts <- list(
  clinical = script_template,
  assay = script_template,
  feature = script_template,
  all = script_template
)

scripts <- list(
  clinical = script_template,
  assay = script_template,
  feature = script_template,
  all = script_template
)

# ** variable formatting --------------------------------------------------
format_string <- function(element) {
  suppressWarnings(if (is.null(element)) {
    return("NULL")
  }
  else if (is.na(element)) {
    return("NA")
  }
  else if (mode(element) == "numeric" ||
           mode(element) == "logical") {
    element <- as.character(element)
  }
  else if (mode(element) == "character") {
    element <-
      sapply(element, function(x) {
        paste0("'", gsub("\\", "\\\\", x, fixed = TRUE), "'")
      }, USE.NAMES = FALSE)
  })
  if (length(element) > 1) {
    element <- paste0("c(", paste(element, collapse = ", "), ")")
  }
  return(element)
}

# ** section headings -----------------------------------------------------
commentify <- function(message) {
  
  comment <- paste0("# ", message, " ")
  comment <- paste0(comment, paste(rep("-", COMMENT_LENGTH - nchar(comment)), collapse = ""))
  c("", "", comment, "")
}

# ** storing lines in a variable ------------------------------------------

write_to_header <- function(lines, overwrite = FALSE) {
  header <<- if (overwrite) lines else c(header, lines)
}

save_lines <- function(lines, datatype, section, overwrite = FALSE) {
  if (length(datatype) > 1 || !datatype %in% ALLOWED_DATATYPES) {
    stop(paste("Error in save_lines.", INVALID_DATATYPE_MESSAGE))
  } else if (length(section) > 1 || !section %in% ALLOWED_SECTIONS) {
    stop(paste("Error in save_lines.", INVALID_SECTION_MESSAGE))
  } else {
    scripts[[datatype]][[section]] <<- if (overwrite) c(script_template[[section]], lines) else c(scripts[[datatype]][[section]], lines)
  }
}

add_library <- function(lib_name, datatype) {
  if (length(datatype) == 1 && datatype %in% ALLOWED_DATATYPES) {
    scripts[[datatype]][["libraries"]] <<- c(scripts[[datatype]][["libraries"]], lib_name)
  } else {
    stop(paste("Error in add_library.", INVALID_DATATYPE_MESSAGE))
  }
}

format_library <- function(lib_name) {
  if (lib_name == "GEOquery") {
    expr_text(substituteDirect(load_geoquery_if_exists, list(lib_name = lib_name)))
  } else {
    expr_text(substituteDirect(load_library_if_exists, list(lib_name = lib_name)))
  }
}

remove_library_if_exists <- function(lib_name, datatype) {
  if (length(datatype) == 1 && datatype %in% ALLOWED_DATATYPES) {
    scripts[[datatype]][["libraries"]] <<- scripts[[datatype]][["libraries"]][scripts[[datatype]][["libraries"]] != lib_name]
  } else {
    stop(paste("Error in remove_library_if_exists.", INVALID_DATATYPE_MESSAGE))
  }
}

add_function <- function(func_name, datatype) {
  if (length(datatype) == 1 && datatype %in% ALLOWED_DATATYPES) {
    # add all dependencies first
    if (!all(is.na(FUNC_LISTS[[func_name]][["lib_dependencies"]]))) {
      add_library(FUNC_LISTS[[func_name]][["lib_dependencies"]], datatype)
    }
    if (!all(is.na(FUNC_LISTS[[func_name]][["func_dependencies"]]))) {
      lapply(FUNC_LISTS[[func_name]][["func_dependencies"]], add_function, datatype = datatype)
    }
    # add function
    scripts[[datatype]][["functions"]] <<- c(scripts[[datatype]][["functions"]], func_name)
  } else {
    stop(paste("Error in add_function.", INVALID_DATATYPE_MESSAGE))
  }
}

format_function <- function(func_name) {
  FUNC_LISTS[[func_name]][["func_text"]]
}

# ** final script ---------------------------------------------------------

set_script_equal <- function(datatype, equal_to) {
  if (all(c(datatype, equal_to) %in% ALLOWED_DATATYPES)) {
    scripts[[datatype]] <<- scripts[[equal_to]]
  } else {
    stop(paste("Error in set_script_equal.", INVALID_DATATYPE_MESSAGE))
  }
}

knit_scripts <- function(datatype1, datatype2, datatype_into) {
  if (!all(c(datatype1, datatype2, datatype_into) %in% ALLOWED_DATATYPES)) {
    stop(paste("Error in knit_scripts.", INVALID_DATATYPE_MESSAGE))
  }
  scripts[[datatype_into]][["libraries"]] <<- union(scripts[[datatype1]][["libraries"]], scripts[[datatype2]][["libraries"]])
  scripts[[datatype_into]][["functions"]] <<- union(scripts[[datatype1]][["functions"]], scripts[[datatype2]][["functions"]])
  scripts[[datatype_into]][["body"]] <<- c(commentify(paste("Formatting", datatype1, "data")),
                                           scripts[[datatype1]][["body"]],
                                           commentify(paste("Formatting", datatype2, "data")),
                                           scripts[[datatype2]][["body"]])
}

save_to_rscript <- function(datatype, file_path = file.path(tempdir(), "script_temp.R")) {
  if (length(datatype) == 1 && datatype %in% ALLOWED_DATATYPES) {
    # format libraries
    libs <- do.call("c", lapply(unique(scripts[[datatype]][["libraries"]]), format_library))
    # format functions
    funcs <- do.call("c", lapply(unique(scripts[[datatype]][["functions"]]), format_function))
    # put all the sections together
    whole_script <- c(
      START_MESSAGE, 
      libs,
      "in_app <- FALSE",
      "",
      funcs, 
      header,
      scripts[[datatype]][["body"]], 
      scripts[[datatype]][["end"]]
    )
    # write script to file
    sink(file_path)
    for (i in 1:length(whole_script)) cat(whole_script[i], fill = T)
    sink()
    
  } else {
    stop(paste("Error in save_to_rscript.", INVALID_DATATYPE_MESSAGE))
  }
}

#  ** undo (for r scripts) ------------------------------------------------

undo_script <- function(datatype) {
  if (length(datatype) == 1 && datatype %in% ALLOWED_DATATYPES) {
    scripts[[datatype]] <<- last_scripts[[datatype]]
  } else {
    stop(paste("Error in undo_script.", INVALID_DATATYPE_MESSAGE))
  }
}

reset_script <- function(datatype) {
  if (length(datatype) == 1 && datatype %in% ALLOWED_DATATYPES) {
    scripts[[datatype]] <<- original_scripts[[datatype]]
  } else {
    stop(paste("Error in reset_script.", INVALID_DATATYPE_MESSAGE))
  }
}

set_undo_point_script <- function(datatype) {
  if (length(datatype) == 1 && datatype %in% ALLOWED_DATATYPES) {
    last_scripts[[datatype]] <<- scripts[[datatype]]
  } else {
    stop(paste("Error in set_undo_point_script.", INVALID_DATATYPE_MESSAGE))
  }
}

set_reset_point_script <- function(datatype) {
  if (length(datatype) == 1 && datatype %in% ALLOWED_DATATYPES) {
    original_scripts[[datatype]] <<- scripts[[datatype]]
  } else {
    stop(paste("Error in set_reset_point_script.", INVALID_DATATYPE_MESSAGE))
  }
}

# Tables ------------------------------------------------------------------

empty_table <- function(this_data) {
  datatable(this_data, rownames = FALSE, colnames = "NO DATA", options = list(dom = "t"))
}

# Graphing ----------------------------------------------------------------

# ** create labels for barplot --------------------------------------------
#shortens values that are too many characters to use as graph labels
shorten_labels <- function(label, max_char) {
  if (is.na(label)) {
    "NA"
  } else if (nchar(label) > max_char) {
    paste0(substr(label, 1, max_char), "...")
  } else {
    label
  }
}

# ** create plot to display -----------------------------------------------
create_plot <- function(value, plot_color, plot_binwidth, title, is_numeric = FALSE) {
  
  if (is_numeric) {
    p <- BASE_HISTOGRAM + 
      geom_histogram(data = data.frame(value = as.numeric(as.character(value))), aes(x = value),
                     fill = plot_color, bins = plot_binwidth) +
      ggtitle(title)
  }
  else {
    p <- BASE_BARPLOT +
      geom_bar(data = as.data.frame(table(value, useNA = "ifany")), aes(x = value, y = Freq), 
               stat = "identity", fill = plot_color) +
      ggtitle(title) +
      scale_x_discrete(labels = sapply(unique(as.character(value)), shorten_labels, 10))
  }
  ggplotly(p) %>% config(displayModeBar = F)
}

# ** create plot to download ----------------------------------------------
create_plot_to_save <- function(variable, plot_color, plot_binwidth, title, is_numeric = FALSE) {
  
  if (is_numeric) {
    BASE_HISTOGRAM + 
      geom_histogram(data = data.frame(measured = as.numeric(as.character(variable))), aes(x = measured),
                     binwidth = plot_binwidth, fill = plot_color) +
      ggtitle(title)
  }
  else {
    BASE_BARPLOT +
      geom_bar(data = as.data.frame(table(variable, useNA = "ifany")), aes(x = variable, y = Freq), 
               stat = "identity", fill = plot_color) +
      ggtitle(title) +
      scale_x_discrete(labels = sapply(unique(as.character(variable)), shorten_labels, 10))
  }
}

graphical_summary_server <- function(datatype, extra_tag = NULL) {
  data_expr <- expr(do.call(view(datatype), list()))
  output[[paste0("choose_", var_to_view(datatype, extra_tag))]] <- renderUI({
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
  output$plots <- renderUI({
    
    if (data_loaded("clinical")) {
      plot_output_list <- lapply(1:ncol(eval(data_expr)), function(i) {
        #if (!grepl("evalSame", colnames(clinical_in_view())[i])) {
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
          div(withSpinner(plotlyOutput(plotname, height = 700, width = "auto"), type = SPINNER_TYPE), tertiary_button(paste0(datatype, "_savePlot", i), div(SAVE_ICON, "Download plot"), class = paste0(datatype, "_plot")))
        }
        #}
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
    if (!is.null(clinical_in_view())) {
      lapply(1:ncol(clinical_in_view()), function(i){
        if (!is_all_identical(clinical_in_view()[,i])) {
          is_all_numeric <- isAllNum(clinical_in_view()[i])
          if (!is_all_unique(clinical_in_view()[,i]) || is_all_numeric) {
            output[[ make.names(colnames(clinical_in_view())[i]) ]] <- renderPlotly({
              suppressWarnings(create_plot(as.character(clinical_in_view()[,i]), input$clinical_plot_color, input$clinical_binwidths, colnames(clinical_in_view())[i], is_all_numeric))
            })
          }
        }
      })
    }
  })
  
  observeEvent(input$last_btn_clinical, {
    if (!is.null(input$last_btn_clinical)) {
      clinical_vals$plot_to_save <- as.numeric(as.character(str_remove(input$last_btn_clinical, "savePlot")))
      showModal(
        modalDialog(
          sliderInput("clinical_plot_width", label = "Image width (cm):", min = 20, max = 100, value = 60),
          sliderInput("clinical_plot_height", label = "Image height (cm):", min = 20, max = 100, value = 60),
          radioButtons("clinical_plot_filetype", label = "File type:", choices = c("PDF" = "pdf", "JPG" = "jpg", "PNG" = "png")),
          downloadButton("clinical_plot_download"),
          footer = modalButton("Close")
        ))
      session$sendCustomMessage(type = "resetValue", "last_btn_clinical")
    }
  })
  
  output$clinical_plot_download <- downloadHandler(
    filename = function() {
      paste(make.names(colnames(clinical_in_view())[clinical_vals$plot_to_save]), input$clinical_plot_filetype, sep = ".")
    },
    content = function(file) {
      plot_to_save <- create_plot_to_save(#plotInput()$total_data[[clinical_vals$plot_to_save]],
        clinical_in_view()[,clinical_vals$plot_to_save], 
        input$clinical_plot_color, 
        input$clinical_binwidths, 
        colnames(clinical_in_view())[clinical_vals$plot_to_save], 
        isAllNum(clinical_in_view()[clinical_vals$plot_to_save]))
      
      ggsave(file, plot_to_save, width = input$clinical_plot_width, height = input$clinical_plot_height, units = "cm", device = input$clinical_plot_filetype)
      
    }
  )
}


# Generating error messages for missing data ------------------------------

get_null_error_message <- function(datatype) {
  if (datatype == "all") {
    HTML('<p style="color:red">No datasets have been joined yet. Please join datasets to view this data.</p>')
  } else {
    HTML(paste0(
      '<p style="color:red">There is no ', datatype, ' data loaded. Please load data in the "',
      cap_first(datatype), 
      ' data" tab.</p>'))
  }
}


# Generating names for files of different types ---------------------------

get_filename <- function(datatype, geoID, filetype) {
  type_names <- list("clinical" = "Annotations", "assay" = "Data", "feature" = "Features", "all" = "Composite")
  if (!datatype %in% names(type_names)) {
    stop(paste("Error in get_filename.", INVALID_DATATYPE_MESSAGE))
  }
  paste0(geoID, "_", type_names[[datatype]], ".", filetype)
}

# Downloading data files --------------------------------------------------

save_data <- function(myData, file, file_type, filenames = NULL) {
  myData <- if (class(myData) == "list") myData else list(myData)
  file_dir <- tempdir()
  files <- if (length(myData) > 1) {
    if (is.null(filenames)) {
      stop("Error in save_data. For zipped files, please specify a list of file names (including the extension) to zip.")
    } else {
      paste0(file_dir, "/", filenames)
    }
  } else {
    file
  }
  lapply(1:length(files), function(i) {
    if (file_type == "csv") {
      write.csv(myData[[i]], files[i], row.names = FALSE)
    }
    else if (file_type == "tsv") {
      write.table(myData[[i]], files[i], sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
    }
    else if (file_type == "JSON") {
      library(jsonlite)
      
      myData[[i]] %>% toJSON() %>% write_lines(files[i])
    }
    else if (file_type == "xlsx") {
      library(xlsx)
      
      write.xlsx(myData[[i]], files[i], row.names = FALSE, showNA = FALSE)
    } else {
      colnames(myData[[i]]) <- str_replace_all(colnames(myData[[i]]), "[\\\\\\/:\\*\\?\\<\\>\\=\\+\\#\\~\\`\\'\\;\\&\\%\\$\\@\\!]", "_")
      write.table(myData[[i]], files[i], sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
    }
  })
  if (length(myData) > 1) {
    current_dir <- getwd()
    setwd(file_dir)
    if (str_detect(file, "tgz")) {
      tar(file, filenames, compression = "gzip")
    } else {
      zip(file, filenames)
    }
    setwd(current_dir)
  }
}

save_rscript <- function(datatype, file, file_name, file_type) {
  file_dir <- tempdir()
  file_names <- if (length(datatype) > 1) paste0(file_dir, "/", file_name, ".R") else file
  lapply(1:length(datatype), function(i) {
    save_lines(commentify("save data"), datatype[i], "end", overwrite = TRUE)
    if (datatype[i] == "clinical") {
      save_lines(c("clinical_data <- cbind(rownames(clinical_data), clinical_data)", 
                   "colnames(clinical_data)[1] <- ''", 
                   paste0("file <- ", format_string(file_name[i]))), 
                 "clinical", "end")
    }
    
    if (file_type == "csv") {
      save_lines(paste0("write.csv(", datatype[i], "_data, file, row.names = FALSE)"), datatype[i], "end")
    }
    else if (file_type == "tsv") {
      save_lines(paste0("write.table(", datatype[i], "_data, file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)"), 
                 datatype[i], "end")
    }
    else if (file_type == "JSON") {
      add_library("jsonlite", datatype[i])
      add_library("readr", datatype[i])
      save_lines(paste0(datatype[i], "_data %>% toJSON() %>% write_lines(file)"), 
                 datatype[i], "end")
    }
    else if (file_type == "xlsx") {
      add_library("xlsx", datatype[i])
      save_lines(paste0("write.xlsx(", datatype[i], "_data, file, row.names = FALSE, showNA = FALSE)"), 
                 datatype[i], "end")
    } else {
      if (datatype == "clinical") {
        save_lines('colnames(myData)[1] <- "ExpId"', "clinical", "end")
      }
      save_lines(rlang::expr_text(rlang::expr(colnames(myData) <- str_replace_all(colnames(myData), "[\\\\\\/:\\*\\?\\<\\>\\=\\+\\#\\~\\`\\'\\;\\&\\%\\$\\@\\!]", "_"))),
                 datatype[i], "end")
      save_lines('write.table(myData, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)',
                 datatype[i], "end")
    }
    
    save_to_rscript(datatype, file_names[i])
  })
  
  if (length(datatype) > 1) {
    current_dir <- getwd()
    setwd(file_dir)
    if (str_detect(file, "tgz")) {
      tar(file, file_names, compression = "gzip")
    } else {
      zip(file, file_names)
    }
    setwd(current_dir)
  }
}

# Deprecated functions ----------------------------------------------------


saveDataRDS <- function(data, fileName) {
  token <- readRDS("droptoken.rds")
  
  filePath <- file.path(tempdir(), fileName)
  write_rds(x = data, path = filePath)
  drop_upload(filePath, path = "Shiny", dtoken = token)
}

loadRdsFromDropbox <- function(geoID) {
  token <- readRDS("droptoken.rds")
  
  currPath <- paste0("/Shiny/", geoID, ".rds")
  localPath <- file.path(tempdir(), paste0(geoID, ".rds"))
  
  filesInfo <- drop_dir("Shiny")
  filePaths <- filesInfo$path_display
  if (currPath %in% filePaths) {
    print("RDS found")
    filePath <- filePaths[which(filePaths == currPath)]
    drop_download(path = filePath, local_path = localPath, dtoken = token, overwrite = TRUE)
    data <- read_rds(localPath)
    return(data)
  }
  return(NULL)
}



