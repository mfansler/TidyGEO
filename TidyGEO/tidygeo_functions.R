
# Load libraries ----------------------------------------------------------


library(readr)
library(GEOquery)
library(stringr)
library(dplyr)
library(tidyr)

# Version -----------------------------------------------------------------


version <- suppressWarnings(readLines("VERSION"))

# R Script writing --------------------------------------------------------


# ** initialize scripts ---------------------------------------------------

script_template <- list(
  header = c(paste("# Script generated using version", version, 
                   "of TidyGEO (https://tidygeo.shinyapps.io/tidygeo/), an"),
             "# application that allows scientists to quickly download and reformat data from",
             "# the online repository Gene Expression Omnibus (GEO).",
             "",
             ""),
  libraries = c(),
  functions = c(),
  body = c(),
  end = c()
)

original_scripts <- list(
  clinical = script_template,
  assay = script_template,
  feature = script_template
)

last_scripts <- list(
  clinical = script_template,
  assay = script_template,
  feature = script_template
)

scripts <- list(
  clinical = script_template,
  assay = script_template,
  feature = script_template
)

# ** helper functions -----------------------------------------------------
func_lists <- readRDS("User/rscript_functions.rds")

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
  
  num_chars <- 75
  comment <- paste0("# ", message, " ")
  comment <- paste0(comment, paste(rep("-", num_chars - nchar(comment)), collapse = ""))
  c("", "", comment, "")
}

# ** storing lines in a variable ------------------------------------------
#saveLines <- function(strings, oFile) {
#  
#  oFile <- c(oFile, strings)
#  
#  return(oFile)
#}

save_lines <- function(lines, datatype = c("clinical", "assay", "feature"), 
                       section = c("header", "body", "end"), overwrite = F) {
  if (length(datatype) > 1 || !datatype %in% c("clinical", "assay", "feature")) {
    stop('Please specify a valid data type ("clinical", "assay", or "feature")')
  } else if (length(section) > 1 || !section %in% c("header", "body", "end")) {
    stop('Please specify a valid section ("header", "body", or "end")')
  } else {
    scripts[[datatype]][[section]] <<- if (overwrite) lines else c(scripts[[datatype]][[section]], lines)
  }
}

add_library <- function(lib_name, datatype = c("clinical", "assay", "feature")) {
  if (length(datatype) == 1 && datatype %in% c("clinical", "assay", "feature")) {
    scripts[[datatype]][["libraries"]] <<- c(scripts[[datatype]][["libraries"]], lib_name)
  } else {
    stop('Please specify a valid data type ("clinical", "assay", or "feature")')
  }
}

format_library <- function(lib_name) {
  c(
    paste0("if (!suppressWarnings(require(", lib_name, ", quietly = TRUE))) {"),
    paste0('  install.packages("', lib_name, '")'),
    paste0("  library(", lib_name, ")"),
    "}"
  )
}

remove_library_if_exists <- function(lib_name, datatype) {
  if (length(datatype) == 1 && datatype %in% c("clinical", "assay", "feature")) {
    scripts[[datatype]][["libraries"]] <<- scripts[[datatype]][["libraries"]][scripts[[datatype]][["libraries"]] != lib_name]
  } else {
    stop('Please specify a valid data type ("clinical", "assay", or "feature")')
  }
}

add_function <- function(func_name, datatype = c("clinical", "assay", "feature")) {
  if (length(datatype) == 1 && datatype %in% c("clinical", "assay", "feature")) {
    scripts[[datatype]][["functions"]] <<- c(scripts[[datatype]][["functions"]], func_name)
    if (!is.na(func_lists[[func_name]][["lib_dependencies"]])) {
      add_library(func_lists[[func_name]][["lib_dependencies"]], datatype)
    }
    if (!is.na(func_lists[[func_name]][["func_dependencies"]])) {
      scripts[[datatype]][["functions"]] <<- c(scripts[[datatype]][["functions"]], func_lists[[func_name]][["func_dependencies"]])
    }
  } else {
    stop('Please specify a valid data type ("clinical", "assay", or "feature")')
  }
}

format_function <- function(func_name) {
  func_lists[[func_name]][["func_text"]]
}

# ** final script ---------------------------------------------------------
#saveToRscript <- function(oFile, version,
#                          filePath = file.path(tempdir(), "script_Temp.R"),
#                          functions) {
#  header <- c(paste("# Script generated using version", version, 
#                    "of TidyGEO (https://tidygeo.shinyapps.io/tidygeo/), an"),
#              "# application that allows scientists to quickly download and reformat data from",
#              "# the online repository Gene Expression Omnibus (GEO).")
#  helper_functions <- 
#    oFile <- c(header, helper_functions, oFile)
#  
#  sink(filePath)
#  for (i in 1:length(oFile)) cat(oFile[i], fill = T)
#  sink()
#}

save_to_rscript <- function(datatype = c("clinical", "asssay", "feature"), file_path = file.path(tempdir(), "script_temp.R")) {
  if (length(datatype) == 1 && datatype %in% c("clinical", "assay", "feature")) {
    # format libraries
    libs <- do.call("c", lapply(unique(scripts[[datatype]][["libraries"]]), format_library))
    # format functions
    funcs <- do.call("c", lapply(unique(scripts[[datatype]][["functions"]]), format_function))
    # put all the sections together
    whole_script <- c(
      scripts[[datatype]][["header"]], 
      libs,
      "in_app <- FALSE",
      "",
      funcs, 
      scripts[[datatype]][["body"]], 
      scripts[[datatype]][["end"]]
    )
    # write script to file
    sink(file_path)
    for (i in 1:length(whole_script)) cat(whole_script[i], fill = T)
    sink()
    
  } else {
    stop('Please specify a valid data type ("clinical", "assay", or "feature")')
  }
}

# ** delete a chunk from the script ---------------------------------------
#removeFromScript <- function(oFile, len, all = F) {
#  length(oFile) <- if (all) len else length(oFile) - len
#  return(oFile)
#}

undo_script <- function(datatype = c("clinical", "assay", "feature")) {
  if (length(datatype) == 1 && datatype %in% c("clinical", "assay", "feature")) {
    scripts[[datatype]] <<- last_scripts[[datatype]]
  } else {
    stop('Please specify a valid data type ("clinical", "assay", or "feature")')
  }
}

reset_script <- function(datatype = c("clinical", "assay", "feature")) {
  if (length(datatype) == 1 && datatype %in% c("clinical", "assay", "feature")) {
    scripts[[datatype]] <<- original_scripts[[datatype]]
  } else {
    stop('Please specify a valid data type ("clinical", "assay", or "feature")')
  }
}

set_undo_point_script <- function(datatype = c("clinical", "assay", "feature")) {
  if (length(datatype) == 1 && datatype %in% c("clinical", "assay", "feature")) {
    last_scripts[[datatype]] <<- scripts[[datatype]]
  } else {
    stop('Please specify a valid data type ("clinical", "assay", or "feature")')
  }
}

set_reset_point_script <- function(datatype = c("clinical", "assay", "feature")) {
  if (length(datatype) == 1 && datatype %in% c("clinical", "assay", "feature")) {
    original_scripts[[datatype]] <<- scripts[[datatype]]
  } else {
    stop('Please specify a valid data type ("clinical", "assay", or "feature")')
  }
}

# Graphing ----------------------------------------------------------------


# ** histogram template ---------------------------------------------------
base_histogram <- ggplot() +
  labs(x = "Values",
       y = "Frequency") +
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5))

# ** barplot template -----------------------------------------------------
base_barplot <- ggplot() +
  labs(x = "Values",
       y = "Count") +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))


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
    p <- base_histogram + 
      geom_histogram(data = data.frame(value = as.numeric(as.character(value))), aes(x = value),
                     fill = plot_color, bins = plot_binwidth) +
      ggtitle(title)
  }
  else {
    p <- base_barplot +
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
    base_histogram + 
      geom_histogram(data = data.frame(measured = as.numeric(as.character(variable))), aes(x = measured),
                     binwidth = plot_binwidth, fill = plot_color) +
      ggtitle(title)
  }
  else {
    base_barplot +
      geom_bar(data = as.data.frame(table(variable, useNA = "ifany")), aes(x = variable, y = Freq), 
               stat = "identity", fill = plot_color) +
      ggtitle(title) +
      scale_x_discrete(labels = sapply(unique(as.character(variable)), shorten_labels, 10))
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



