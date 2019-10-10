
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
    else if (file_type == "json") {
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
      save_lines(c("myData <- cbind(rownames(clinical_data), clinical_data)", 
                   "colnames(myData)[1] <- ''", 
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
    else if (file_type == "json") {
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



