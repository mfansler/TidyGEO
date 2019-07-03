
# libraries ---------------------------------------------------------------


if (!require(GEOquery, quietly = TRUE)) {
  source("https://bioconductor.org/biocLite.R")
  BiocInstaller::biocLite("GEOquery")
  library(GEOquery)
}
if (!require(stringr, quietly = TRUE)) {
  install.packages("stringr")
  library(stringr)
}
if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(tidyr, quietly = TRUE)) {
  install.packages("tidyr")
  library(tidyr)
}

# helper functions --------------------------------------------------------


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
  return(GEOquery:::parseGSEMatrix(destfile, destdir = destdir, 
                                   AnnotGPL = AnnotGPL, getGPL = getGPL)$eset)
}

load_series <- function(geoID, platform, session = NULL) {
  
  expressionSet <- getGEO(geoID, platform = platform)
  
  return(expressionSet)
}

process_expression <- function(expressionSet) {
  
  expression_raw <- assayData(expressionSet)$exprs
  #expressionData <- data.frame("ID" = rownames(expressionData), expressionData)
  pass <- tryCatch({
    expressionData <- data.frame("ID" = rownames(expression_raw), apply(expression_raw, 2, as.numeric))
    TRUE
  }, warning = function(w) {
    if (grepl("NAs introduced by coercion", w)) {
      FALSE
    }
  })
  #browser()
  if (!pass) {
    expressionData <- data.frame("ID" = rownames(expression_raw), expression_raw)
    print(paste("Some of the data is non-numeric.",
                "This data is not supported by our some of our functionality.",
                "Feel free to download the data to edit with another application."))
  }
  featureData <- data.frame(fData(expressionSet))
  if (!"ID" %in% colnames(featureData)) {
    featureData <- cbind(ID = rownames(featureData), featureData)
    
    rows_to_keep <- sapply(1:nrow(featureData), function(i) {
      if (!all(is.na(featureData[i,]))) {
        i
      }
    })
    featureData <- featureData[unlist(rows_to_keep),]
  }
  
  if (nrow(expressionData) == 0) {
    expressionData <- NULL
  }
  if (nrow(featureData) == 0) {
    featureData <- NULL
  }
  
  return(list("expressionData" = expressionData, "featureData" = featureData, "status" = pass))
}

quickTranspose <- function(dataToTranspose) {
  
  if (any(duplicated(dataToTranspose$ID, incomparables = NA))) {
    transposed <- dataToTranspose
    
    print("Data cannot be transposed because the ID column is not all unique. Please specify a summarize option.")
    
  } else {
    transposed <- dataToTranspose %>%
      gather(newrows, valname, -ID) %>%
      spread(ID, valname) %>%
      dplyr::rename(ID = "newrows")
  }
  
  return(transposed)
}

replaceID <- function(data, replacement, replaceCol, summaryOption, dropNA) {
  
  if (dropNA) {
    replacementWRowNames <- replacement[!is.na(replacement[,replaceCol]), c("ID", replaceCol)]
  } else {
    replacementWRowNames <- replacement[, c("ID", replaceCol)]
  }
  replacementWRowNames$ID <- as.character(replacementWRowNames$ID)
  
  mergedData <- inner_join(data, replacementWRowNames, by = "ID") %>%
    select(-ID) %>%
    dplyr::rename(ID = replaceCol) %>%
    select(ID, everything())
  
  if (!is.null(summaryOption)) {
    if (summaryOption == "mean") {
      mergedData <- mergedData %>% group_by(ID) %>%
        summarize_all(mean, na.rm = TRUE) %>%
        ungroup()
    } else if (summaryOption == "median") {
      mergedData <- mergedData %>% group_by(ID) %>%
        summarize_all(median, na.rm = TRUE) %>%
        ungroup()
    } else if (summaryOption == "max") {
      mergedData <- mergedData %>% group_by(ID) %>%
        summarize_all(max, na.rm = TRUE) %>%
        ungroup()
    } else if (summaryOption == "min") {
      mergedData <- mergedData %>% group_by(ID) %>%
        summarize_all(min, na.rm = TRUE) %>%
        ungroup()
    }
  }
  
  return(mergedData)
}

filterExpressionData <- function(data, shinyFilterSpecs) {
  for (i in 1:length(shinyFilterSpecs)) {
    if (shinyFilterSpecs[[i]] != "") {
      col_index <- which(colnames(data) == names(shinyFilterSpecs)[i])
      if (grepl("\"", shinyFilterSpecs[[i]])) {
        searchStrs <- shinyFilterSpecs[[i]] %>%
          str_remove_all("\"") %>%
          str_remove_all("\\[") %>%
          str_remove_all("\\]") %>%
          str_split(",")
        searchStrs <- searchStrs[[1]]
        data <- data[data[,col_index] %in% searchStrs,]
      } else if (grepl(" ... ", shinyFilterSpecs[[i]])) {
        searchStrs <- as.numeric(str_split(shinyFilterSpecs[[i]], " ... ")[[1]])
        data <- data[data[,col_index] >= searchStrs[1] & data[,col_index] <= searchStrs[2],]
      } else {
        data <- data[grepl(shinyFilterSpecs[[i]], unlist(data[,col_index]), ignore.case = TRUE),]
      }
    }
  }
  return(data)
}

find_intersection <- function(data1, data2, id_col1 = "ID", id_col2 = "ID") {
  #if (id_col1 != "ID") {
  #  browser()
  #}
  search_terms <- if (id_col2 == "colnames") c(colnames(data2)) else unlist(data2[,id_col2])
  
  if (id_col1 == "colnames") {
    data1[,which(colnames(data1) %in% c(search_terms, "ID"))]
  } else {
    data1[which(data1[,id_col1] %in% search_terms),]
  }
}