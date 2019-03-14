library(rdrop2)
library(readr)
library(GEOquery)
library(stringr)
library(dplyr)
library(tidyr)

# detects variable type & formats string to be written to R script --------


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
        paste0("'", x, "'")
      }, USE.NAMES = FALSE)
  })
  if (length(element) > 1) {
    element <- paste0("c(", paste(element, collapse = ", "), ")")
  }
  return(element)
}

# creates section headings for R script -----------------------------------


commentify <- function(message) {
  
  num_chars <- 75
  comment <- paste0("# ", message, " ")
  comment <- paste0(comment, paste(rep("-", num_chars - nchar(comment)), collapse = ""))
  c("", "", comment, "")
}

base_histogram <- ggplot() +
  labs(x = "Values",
       y = "Frequency") +
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5))

base_barplot <- ggplot() +
  labs(x = "Values",
       y = "Count") +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))

saveLines <- function(strings, oFile) {
  
  oFile <- c(oFile, strings)
  
  return(oFile)
}

saveToRscript <- function(oFile, filePath = file.path(tempdir(), "script_Temp.R"), 
                          functions_path = 'User/clinical_helper_functions.R') {
  file.copy(functions_path, filePath, overwrite = TRUE)
  sink(filePath, append = TRUE)
  for (i in 1:length(oFile)) cat(oFile[i], fill = T)
  sink()
}

removeFromScript <- function(oFile, len, all = F) {
  length(oFile) <- if (all) len else length(oFile) - len
  return(oFile)
}

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

getGEO <- function(GEO = NULL, filename = NULL, destdir = tempdir(), 
                    GSElimits = NULL, GSEMatrix = TRUE, AnnotGPL = FALSE, getGPL = TRUE, 
                    parseCharacteristics = TRUE, platform = NULL) 
{
  con <- NULL
  if (!is.null(GSElimits)) {
    if (length(GSElimits) != 2) {
      stop("GSElimits should be an integer vector of length 2, like (1,10) to include GSMs 1 through 10")
    }
  }
  if (is.null(GEO) & is.null(filename)) {
    stop("You must supply either a filename of a GEO file or a GEO accession")
  }
  incProgress()
  if (is.null(filename)) {
    GEO <- toupper(GEO)
    geotype <- toupper(substr(GEO, 1, 3))
    if (GSEMatrix & geotype == "GSE") {
      return(getAndParseGSEMatrices(GEO, destdir, AnnotGPL = AnnotGPL, 
                                    getGPL = getGPL, parseCharacteristics = parseCharacteristics, platform = platform))
    }
    filename <- getGEOfile(GEO, destdir = destdir, AnnotGPL = AnnotGPL)
  }
  ret <- parseGEO(filename, GSElimits, destdir, AnnotGPL = AnnotGPL, 
                  getGPL = getGPL)
  return(ret)
}
getAndParseGSEMatrices <- function(GEO, destdir, AnnotGPL, getGPL = TRUE, 
                                    parseCharacteristics = TRUE, platform = NULL) 
{
  GEO <- toupper(GEO)
  stub = gsub("\\d{1,3}$", "nnn", GEO, perl = TRUE)
  if (is.null(platform)) {
    gdsurl <- "https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/"
    b = getDirListing(sprintf(gdsurl, stub, GEO))
    platform <- b[1]
  }
  incProgress()
  destfile = file.path(destdir, platform)
  if (file.exists(destfile)) {
    message(sprintf("Using locally cached version: %s", 
                    destfile))
  }
  else {
    download.file(sprintf("https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/%s", 
                          stub, GEO, platform), destfile = destfile, mode = "wb", 
                  method = getOption("download.file.method.GEOquery"))
  }
  incProgress()
  return(GEOquery:::parseGSEMatrix(destfile, destdir = destdir, 
                        AnnotGPL = AnnotGPL, getGPL = getGPL)$eset)
}

get_platforms <- function(geoID, session = NULL) {
  platforms <- NULL
  status <- tryCatch({
    GEO <- toupper(geoID)
    stub = gsub("\\d{1,3}$", "nnn", GEO, perl = TRUE)
    gdsurl <- "https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/"
    #browser()
    platforms = GEOquery:::getDirListing(sprintf(gdsurl, stub, GEO))
    "pass"
  }, error = function(e) {
    if (str_detect(paste0(e), "open\\.connection.*HTTP error 404.")) {
      return("File not found. Please enter a valid ID.")
    } else {
      return(paste(e))
    }
  })
  if (status != "pass" && !is.null(session)) {
    createAlert(session, "alert", "fileError", title = "Error",
                content = unlist(status), append = FALSE)
  }
  return(platforms)
}

load_series <- function(geoID, platform, session = NULL) {
  
  #expressionSet <- loadRdsFromDropbox(geoID)
  expressionSet <- NULL
  
  if (is.null(expressionSet)) {
    status <- tryCatch({
      if (!grepl("GSE", geoID)) {
        stop('Please enter an ID that begins with "GSE".', call. = FALSE)
      }
      #expressionSet <- getGEO(GEO = geoID, GSEMatrix = TRUE, getGPL = TRUE, AnnotGPL = TRUE)
      #browser()
      incProgress()
      expressionSet <- getGEO(geoID, platform = platform)
      #saveDataRDS(expressionSet, paste0(geoID, ".rds"))
      "pass"
    }, error = function(e){
        if (grepl("open\\.connection", paste0(e))) {
          return(paste0("Trouble establishing connection to GEO. Please try again later."))
        }
        else if (grepl("file\\.exists", paste0(e))) {
          return(paste0("File not found. Please enter a valid ID."))
        }
        else {
          return(paste(e))
        }
      }
    )
    incProgress()
    if (status != "pass" && !is.null(session)) {
      title <- "Error"
      content <- unlist(status)
      createAlert(session, "alert", "fileError", title = "Error",
                  content = unlist(status), append = FALSE)
    } else {
      title <- "Success!"
      content <- "Series data successfully downloaded. Please continue to Clinical data and Assay data tabs
      to see the data."
    }
    createAlert(session, "alert", "fileError", title = title,
                content = content, append = FALSE)
  }
  
  return(expressionSet)
}

isAllNum <- function(metaData) {
  #toEvaluate <- metaData[which(!is.na(metaData)),]
  toEvaluate <- na.omit(unlist(metaData))
  vals <- unique(toEvaluate)
  temp <- suppressWarnings(as.numeric(as.character(toEvaluate)))
  isNum <- all(is.numeric(temp)) && all(!is.na(temp)) && length(vals) > 2
  return(isNum)
}

is_all_unique <- function(my_list) {
  vals <- unique(my_list[which(!is.na(my_list))])
  return(length(vals) == length(my_list))
}

is_all_identical <- function(my_list) {
  vals <- unique(my_list[which(!is.na(my_list))])
  return(length(vals) == 1)
}

replace_blank_cells <- function(values) {
  str_replace(values, "^ *$", NA_character_)
}

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

create_plot <- function(variable, plot_color, plot_binwidth, title, is_numeric = FALSE) {
  
  if (is_numeric) {
    p <- base_histogram + 
      geom_histogram(data = data.frame(measured = as.numeric(as.character(variable))), aes(x = measured),
                     binwidth = plot_binwidth, fill = plot_color) +
      ggtitle(title)
  }
  else {
    p <- base_barplot +
      geom_bar(data = as.data.frame(table(variable, useNA = "ifany")), aes(x = variable, y = Freq), 
               stat = "identity", fill = plot_color) +
      ggtitle(title) +
      scale_x_discrete(labels = sapply(unique(as.character(variable)), shorten_labels, 10))
  }
  ggplotly(p) %>% config(displayModeBar = F)
}

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
