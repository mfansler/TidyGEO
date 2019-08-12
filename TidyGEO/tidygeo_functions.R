
# Load libraries ----------------------------------------------------------


library(readr)
library(GEOquery)
library(stringr)
library(dplyr)
library(tidyr)

# Version -----------------------------------------------------------------


version <- suppressWarnings(readLines("VERSION"))

# Small utility functions -------------------------------------------------

cap_first <- function(my_str) {
  paste0(toupper(substring(my_str, 1, 1)), substring(my_str, 2, nchar(my_str)))
}

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
  if (lib_name == "GEOquery") {
    c(
      paste0("if (!suppressWarnings(require(", lib_name, ", quietly = TRUE))) {"),
      '  source("https://bioconductor.org/biocLite.R")',
      '  BiocInstaller::biocLite("GEOquery")',
      paste0("  library(", lib_name, ")"),
      "}"
    )
  } else {
    c(
      paste0("if (!suppressWarnings(require(", lib_name, ", quietly = TRUE))) {"),
      paste0('  install.packages("', lib_name, '")'),
      paste0("  library(", lib_name, ")"),
      "}"
    ) 
  }
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
    # add all dependencies first
    if (!all(is.na(func_lists[[func_name]][["lib_dependencies"]]))) {
      add_library(func_lists[[func_name]][["lib_dependencies"]], datatype)
    }
    if (!all(is.na(func_lists[[func_name]][["func_dependencies"]]))) {
      lapply(func_lists[[func_name]][["func_dependencies"]], add_function, datatype = datatype)
    }
    # add function
    scripts[[datatype]][["functions"]] <<- c(scripts[[datatype]][["functions"]], func_name)
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
    stop('Please specify a valid data type ("clinical", "assay", "feature", or "all)')
  }
  paste0(geoID, "_", type_names[[datatype]], ".", filetype)
}

# Downloading data files --------------------------------------------------

save_data <- function(myData, file, file_type, filenames = NULL) {
  myData <- if (class(myData) == "list") myData else list(myData)
  file_dir <- tempdir()
  files <- if (length(myData) > 1) {
    if (is.null(filenames)) {
      stop("For zipped files, please specify a list of file names (including the extension) to zip.")
    } else {
      lapply(filenames, function(filename) {
        paste0(file_dir, "/", filename)
      })
    }
  } else {
    list(file)
  }
  lapply(1:length(files), function(i) {
    if (file_type == "csv") {
      write.csv(myData[[i]], files[[i]], row.names = FALSE)
    }
    else if (file_type == "tsv") {
      write.table(myData[[i]], files[[i]], sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
    }
    else if (file_type == "JSON") {
      library(jsonlite)
      
      myData[[i]] %>% toJSON() %>% write_lines(files[[i]])
    }
    else if (file_type == "xlsx") {
      library(xlsx)
      
      write.xlsx(myData[[i]], files[[i]], row.names = FALSE, showNA = FALSE)
    } else {
      colnames(myData[[i]]) <- str_replace_all(colnames(myData[[i]]), "[\\\\\\/:\\*\\?\\<\\>\\=\\+\\#\\~\\`\\'\\;\\&\\%\\$\\@\\!]", "_")
      write.table(myData[[i]], files[[i]], sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
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
  save_lines(commentify("save data"), datatype, "end", overwrite = TRUE)
  if (datatype == "clinical") {
    save_lines(c("clinical_data <- cbind(rownames(clinical_data), clinical_data)", 
                 "colnames(clinical_data)[1] <- ''", 
                 paste0("file <- ", format_string(file_name))), 
               "clinical", "end")
  }
  
  if (file_type == "csv") {
    save_lines(paste0("write.csv(", datatype, "_data, file, row.names = FALSE)"), datatype, "end")
  }
  else if (file_type == "tsv") {
    save_lines(paste0("write.table(", datatype, "_data, file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)"), 
               datatype, "end")
  }
  else if (file_type == "JSON") {
    add_library("jsonlite", datatype)
    add_library("readr", datatype)
    save_lines(paste0(datatype, "_data %>% toJSON() %>% write_lines(file)"), 
               datatype, "end")
  }
  else if (file_type == "xlsx") {
    add_library("xlsx", datatype)
    save_lines(paste0("write.xlsx(", datatype, "_data, file, row.names = FALSE, showNA = FALSE)"), 
               datatype, "end")
  } else {
    if (datatype == "clinical") {
      save_lines('colnames(myData)[1] <- "ExpId"', "clinical", "end")
    }
    save_lines(rlang::expr_text(rlang::expr(colnames(myData) <- str_replace_all(colnames(myData), "[\\\\\\/:\\*\\?\\<\\>\\=\\+\\#\\~\\`\\'\\;\\&\\%\\$\\@\\!]", "_"))),
               datatype, "end")
    save_lines('write.table(myData, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)',
               datatype, "end")
  }
  
  save_to_rscript(datatype, file)
}

save_assay_data <- function(file) {
  
  if (input$expression_fileType == "csv") {
    
    #print("exprToDisplay")
    #print(values$exprToDisplay, n = 10)
    withProgress(message = "Writing data to file", {
      incProgress()
      write.csv(assay_vals$assay_data, file, row.names = FALSE)
      incProgress()
    })
  }
  else if (input$expression_fileType == "tsv") {
    withProgress(message = "Writing data to file",
                 write.table(assay_vals$assay_data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
  }
  else if (input$expression_fileType == "JSON") {
    library(jsonlite)
    #library(readr)
    
    withProgress(message = "Writing data to file", {
      incProgress()
      assay_vals$assay_data %>% toJSON() %>% write_lines(file)
      incProgress()
    })
    
  }
  else if (input$expression_fileType == "xlsx") {
    library(xlsx)
    
    withProgress(message = "Writing data to file", {
      incProgress()
      write.xlsx(assay_vals$assay_data, file, row.names = FALSE, showNA = FALSE)
      incProgress()
    })
  } else {
    #files <- c(paste0(tempfile(), input$geoID, "_Data.txt"), paste0(tempfile(), input$geoID, "_GeneAnnotations.txt"))
    myData <- assay_vals$assay_data
    colnames(myData) <- colnames(myData) <- str_replace_all(colnames(myData), "[\\\\\\/:\\*\\?\\<\\>\\=\\+\\#\\~\\`\\'\\;\\&\\%\\$\\@\\!]", "_")
    withProgress(message = "Writing data to file",
                 write.table(myData, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
    #withProgress(message = "Writing feature data to file",
    #             write.table(assay_vals$feature_data, files[2], sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
    #zip(file, files)
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



