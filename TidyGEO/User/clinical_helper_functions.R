
# libraries ---------------------------------------------------------------


library(GEOquery)
library(stringr)
library(dplyr)
library(tidyr)

# helper functions --------------------------------------------------------


saveData <- function(metaData, outputRawFilePath) {
  if (!dir.exists(dirname(outputRawFilePath)))
    dir.create(dirname(outputRawFilePath), recursive = TRUE, showWarnings = FALSE)
  
  write.table(metaData, outputRawFilePath, sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)
}

loadData <- function(fileName) {
  
  if (file.exists(fileName)) {
    metaData <- read.table(fileName, sep = "\t", row.names = NULL, header = TRUE, na.strings = "NA", check.names = FALSE, quote = NULL)
    colnames(metaData)[1] <- "SampleID"
    return(metaData)
  }
  else {
    return(NULL)
  }
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
    b = GEOquery:::getDirListing(sprintf(gdsurl, stub, GEO))
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

process_clinical <- function(expressionSet, session = NULL) {
  
  metaData <- pData(expressionSet)
  if (nrow(metaData) == 1) {
    metaData <- as.data.frame(t(as.matrix(apply(metaData, 2, replace_blank_cells))), row.names = rownames(metaData), stringsAsFactors = FALSE)
  } else {
    metaData <- as.data.frame(apply(metaData, 2, replace_blank_cells), row.names = rownames(metaData), stringsAsFactors = FALSE)
  }
  filtered_data <- filterUninformativeCols(metaData)
  if (!is.null(filtered_data)) {
    metaData <- filtered_data
  }
  
  return(metaData)
}

#filter columns with all different entries or all the same entry
evaluate_cols_to_keep <- function(col, toFilter = list()) {
  functions <- list("reanalyzed" = function(x) all(!grepl("Reanaly[sz]ed ", x)),
                    "url" = function(x) all(!grepl("ftp:\\/\\/", x)),
                    "dates" = function(x) all(!grepl("[A-Za-z]+ [0-9]{1,2},? [0-9]{2,4}", x)),
                    "same_vals" = function(x) length(unique(as.factor(as.character(toupper(x))))) > 1,
                    "all_diff" = function(x) length(unique(as.factor(as.character(toupper(x))))) != total_rows,
                    "tooLong" = function(x) {
                      isTooLong <- as.logical(nchar(x) > 100)
                      sum(isTooLong) < (length(x) / 2)
                    })
  col_no_NA <- if (any(is.na(col)) || any(col == "")) col[-which(is.na(col) | col == "")] else col
  total_rows <- length(col)
  if (length(col_no_NA) > 0) {
    if (length(toFilter > 0)) {
      return(all(sapply(toFilter, function(x) {
        do.call(what = functions[x][[1]], args = list("x" = col_no_NA))
      })))
    }
    return(TRUE)
  }
  return(FALSE)
}

filterUninformativeCols <- function(metaData, toFilter = list())
{
  metaData <- metaData[!duplicated(as.list(metaData))]
  
  if (ncol(metaData) > 1) {
    
    cols_to_keep <- apply(metaData, 2, evaluate_cols_to_keep, toFilter = toFilter)
    
    if (all(!cols_to_keep)) {
      print("No informative columns found.")
      NULL
    } else {
      metaData <- metaData[,cols_to_keep]
    }
  }
  metaData
}

isAllNum <- function(metaData) {
  vals <- unique(metaData[,1])
  temp <- suppressWarnings(as.numeric(as.character(metaData[,1])))
  isNum <- all(is.numeric(temp)) && all(!is.na(temp)) && length(vals) > 2
  return(isNum)
}

isAllUnique <- function(metaData) {
  vals <- unique(metaData[,1])
  return(length(vals) == nrow(metaData))
}

saveFileDescription <- function(geoID, filePathToSave) {
  
  desFilePath <- paste(filePathToSave, "_Description.md", sep = "")
  if (!file.exists(desFilePath)) {  
    url <- paste("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", geoID, "&targ=self&form=text&view=quick", sep = "")
    tempFile <- paste("Clinical_Raw/", geoID, "__ncbi.txt", sep = "")
    if (!file.exists(tempFile)) {
      download.file(url, tempFile, method = "auto")
    }
    summaryFile <- suppressMessages(suppressWarnings(read.table(tempFile, sep = "\t", col.names = "data", quote = NULL)))
    
    toFind <- c("!Series_title", "!Series_platform_organism", "!Series_type", "!Series_summary", "!Series_pubmed_id")
    titles <- c("## Title", "## Organism", "## Experiment type", "## Summary", "## Citation")
    valuesList <- NULL
    for (term in toFind) {
      value = NULL
      searchResults <- summaryFile[which(grepl(term, summaryFile$data)),]
      if (length(searchResults) == 0) {
        value = "NA"
      }
      else {
        for (item in searchResults) {
          item <- toString(item)
          item <- str_split(item, " = ")
          value <- str_trim(paste(value, item[[1]][2], sep = " "))
        }
        if (term == "!Series_pubmed_id") {
          value <- paste("[PubMed article]", "(https://www.ncbi.nlm.nih.gov/pubmed/", value, ")", sep = "")
        }
      }
      valuesList <- append(valuesList, value)
    }
    
    if (!dir.exists(dirname(desFilePath)))
      dir.create(dirname(desFilePath), recursive = TRUE)
    
    for (i in 1:length(titles)) {
      write(c(titles[i], valuesList[i]), file = desFilePath, append = TRUE, sep = "\n")
      write("", file = desFilePath, append = TRUE, sep = "\n")
    }
  }
}

shift_cells <- function(data, col1, col2, conflicts = NULL) {
  if (is.null(conflicts)) {
    data <- unite(data, col2, col1, col = !!col2)
    data[,col2] <- str_remove_all(data[,col2], "_NA|NA_")
  } else if (conflicts == col1) {
    shift_indices <- which(!is.na(data[,col1]))
    data[shift_indices, col2] <- data[shift_indices, col1]
    data[shift_indices, col1] <- rep(NA, length(shift_indices))
  } else if (conflicts == col2) {
    shift_indices <- which(is.na(data[,col2]))
    data[shift_indices, col2] <- data[shift_indices, col1]
    data[shift_indices, col1] <- rep(NA, length(shift_indices))
  } else {# conflicts = delimiter
    data <- unite(data, col1, col2, col = !!col2, sep = conflicts)
    data[,col2] <- str_remove_all(data[,col2], paste0(conflicts, "NA|NA", conflicts))
  }
  results <- data
  return(results)
}

extractColNames <- function(input_df, delimiter, colsToSplit, use_regex = FALSE) {
  
  if (is.null(colsToSplit) || delimiter == "") {
    return(input_df) 
  }
  
  errorMessage <- NULL
  
  input_df <- cbind(row_names = rownames(input_df), input_df)
  num_split <- 0
  
  regex_delimiter <- if (!use_regex) paste0("\\Q", delimiter, "\\E") else delimiter
  
  tryCatch({
    for (col in colsToSplit) {
      hasDelim <- is.na(input_df[, col]) | str_detect(input_df[, col], regex_delimiter)
      
      if (all(hasDelim)) {
        
        metaData <- separate(metaData, col, sep = regex_delimiter, into = c("key", "value"), extra = "merge") %>%
          mutate(key = str_trim(key), value = str_trim(value))
        
        if (any(metaData$key %in% colnames(metaData))) {
          metaData <- metaData %>%
            group_by(key) %>%
            mutate(key_mod = if (unique(key) %in% colnames(metaData)) paste0(key, ".1") else key) %>%
            ungroup() %>%
            select(-key) %>%
            rename(key = "key_mod")
        }
        
        col_names <- colnames(metaData)
        col_names <- append(col_names, unique(metaData$key)[which(!is.na(unique(metaData$key)))], which(col_names == "key"))
        col_names <- col_names[-which(col_names %in% c("key", "value"))]
        
        metaData <- spread(metaData, key = "key", value = "value")
        metaData <- metaData[,col_names]
        
        num_split <- num_split + 1
      } else {
        
        offendingRows <- paste((1:length(hasDelim))[!hasDelim], collapse = ", ")
        offendingRows <- if_else(nchar(offendingRows) > 50, paste0(substr(offendingRows, 1, 50), "... "), offendingRows)
        
        offendingVals <- paste(metaData[!hasDelim, col], collapse = ", ")
        offendingVals <- if_else(nchar(offendingVals) > 50, paste0(substr(offendingVals, 1, 50), "... "), offendingVals)
        
        errorMessage <- c(errorMessage, paste0(col, " could not be split."),
                          paste0("Rows: ", offendingRows),
                          paste0("Values: ", offendingVals))
      }
    }
  }, error = function(e) {
    errorMessage <<- 'Something went wrong. Try again with use_regex set to FALSE.'
    metaData <<- input_df
  })
  
  
  if (!is.null(errorMessage)) {
    if (!str_detect(errorMessage, 'Something went wrong')) {
      errorMessage <- c(paste0('Looks like there are some cells that don\'t contain the delimiter "', delimiter, '".'),
                        errorMessage)
    }
    print(errorMessage)
  }
  
  metaData <- data.frame(metaData[,-1], row.names = metaData$row_names, check.names = FALSE)
  
  if (num_split > 0) {
    colnames(metaData) <- col_names[-1]
    metaData <- filterUninformativeCols(metaData)
  }
  
  return(metaData)
}

splitCombinedVars <- function(input_df, colsToDivide, delimiter, use_regex = FALSE) {
  if (is.null(colsToDivide) || delimiter == "") {
    return(input_df)
  }
  metaData <- input_df
  num_split <- 0
  errorMessage <- NULL
  
  regex_delimiter <- if (!use_regex) paste0("\\Q", delimiter, "\\E") else delimiter
  
  tryCatch({
    for (colName in colsToDivide) {
      numElements <- max(sapply(metaData[,colName], function(x) {
        len <- length(str_extract_all(x, regex_delimiter)[[1]]) + 1
        if (len > nchar(x)) {
          stop("You have made a mistake.")
        } else {
          len
        }
      }), na.rm = TRUE)
      #targetCol <- input_df[,colName]
      if (numElements > 1) {
        
        colNames <- NULL
        for (i in 1:numElements) {
          colNames <- c(colNames, paste(colName, i, sep = "."))
        }
        metaData <- separate(metaData, col = colName, into = colNames, sep = regex_delimiter)
        
        num_split <- num_split + 1
      } else {
        offendingVals <- paste(metaData[, colName], collapse = ", ")
        offendingVals <- if_else(nchar(offendingVals) > 50, paste0(substr(offendingVals, 1, 50), "... "), offendingVals)
        
        errorMessage <- c(errorMessage, paste0(colName, " could not be split."),
                          paste0("Values: ", offendingVals))
      }
    }
  }, error = function(e) {
    errorMessage <<- 'Something went wrong. Try again with use_regex set to FALSE.'
    metaData <<- input_df
  })
  
  
  if (!is.null(errorMessage)) {
    if (!str_detect(errorMessage, 'Something went wrong')) {
      errorMessage <- c(paste0('Looks like there are some cells that don\'t contain the delimiter "', delimiter, '".'),
                        errorMessage)
    }
    print(errorMessage)
  }
  
  if (num_split > 0) {
    metaData <- filterUninformativeCols(metaData)
  }
  
  return(metaData)
}

filterCols <- function(metaData, varsToKeep) {
  if (length(varsToKeep) > 0) {
    metaData <- metaData[which(colnames(metaData) %in% varsToKeep)]
  }
  return(metaData)
}

findOffendingChars <- function(x){
  offendingChars <- c("?", "$", "/", "#", "=", "'", "%", "*", "^", "@", "!", "&", "(", ")")
  myChars <- NULL
  for (char in offendingChars) {
    myChars[[char]] <- (grepl(paste0("\\", char), x))
  }
  return(myChars)
}

replace_blank_cells <- function(values) {
  for (pattern in c("^ $", "^$")) {
    values <- str_replace(values, pattern, NA_character_)
  }
  values
}

renameCols <- function(metaData, old_name, new_name) {
  if (new_name %in% colnames(metaData)) {
    print(paste0('Cannot name ', old_name, ' "', new_name, 
                 '" because this would create duplicate column names, which is not allowed.'))
  } else if (old_name %in% colnames(metaData)) {
    #newNames[1] <- str_replace_all(unname(newNames), "[^\._\s0-9A-Za-z]", ".")
    colnames(metaData)[which(colnames(metaData) == old_name)] <- new_name
  }
  
  return(metaData)
}


substitute_vals <- function(clinical_data, sub_specs, use_reg_ex = FALSE)
{
  col_to_sub <- names(sub_specs) 
  subs <- sub_specs[[col_to_sub]]
  row_names <- rownames(clinical_data)
  clinical_data[,col_to_sub] <- as.character(clinical_data[,col_to_sub])
  
  if (any(subs$New_Val == "NA")) {
    subs$New_Val[which(subs$New_Val == "NA")] <- NA
  }
  
  for (i in 1:nrow(subs)) {
    if (is.na(subs$To_Replace[i]) || subs$To_Replace[i] == "") {
      na_rows <- which(is.na(clinical_data[,col_to_sub]) | clinical_data[,col_to_sub] == "")
      if (length(na_rows) > 0) {
        clinical_data[na_rows, col_to_sub] <- rep(subs$New_Val[i], length(na_rows))
      }
    } else if (grepl("RANGE", subs$To_Replace[i])) {
      
      mySub <- str_remove(subs$To_Replace[i], "RANGE: ")
      mySub <- as.numeric(str_split(mySub, " - ")[[1]])
      
      new_col <- suppressWarnings(as.numeric(as.character(clinical_data[,col_to_sub])))
      new_col[
        which(!is.na(new_col) & 
                mySub[1] <= new_col &
                new_col <= mySub[2])
        ] <- as.character(subs$New_Val[i])
      
      clinical_data[which(!is.na(new_col)), col_to_sub] <- new_col[which(!is.na(new_col))]
      
    } else {
      
      clinical_data[,col_to_sub] <- gsub(clinical_data[,col_to_sub], 
                                         pattern = subs$To_Replace[i], 
                                         replacement = subs$New_Val[i], 
                                         fixed = !use_reg_ex)
    }
  }
  clinical_data <- clinical_data %>% mutate_all(~ replace(., . == "", NA))
  rownames(clinical_data) <- row_names
  return(clinical_data)
}

excludeVars <- function(metaData, variable, to_exclude) {
  metaData <- cbind(ID = rownames(metaData), metaData)
  tryCatch({ #for debugging purposes, take out in final product
    metaData <- dplyr::rename(metaData, filter_var = variable)
    if (any(to_exclude == "NA")) {
      metaData <- filter(metaData, !is.na(filter_var))
    }
    if (any(!to_exclude %in% metaData$filter_var)) { #indicates that the thing to exclude is a range
      values <- to_exclude[which(!to_exclude %in% metaData$filter_var)]
      if (grepl("exclude", values)) {
        el <- str_split(values, "exclude: ")[[1]][2]
        bounds <- as.numeric(str_split(el, " - ")[[1]])
        metaData <- metaData %>%
          within({
            filter_var <- as.numeric(filter_var)
          }) %>%
          dplyr::filter(filter_var < bounds[1] | filter_var > bounds[2])
      }
      else if (grepl("keep", values)) {
        el <- str_split(values, "keep: ")[[1]][2]
        bounds <- as.numeric(str_split(el, " - ")[[1]])
        metaData <- metaData %>%
          within({
            filter_var <- as.numeric(filter_var)
          }) %>%
          dplyr::filter(filter_var >= bounds[1], filter_var <= bounds[2])
      }
    }
    #keep these lines down here so it won't fool the if statement above
    values <- to_exclude[which(to_exclude %in% metaData$filter_var)]
    metaData <- if (!identical(values, character(0))) filter(metaData, !filter_var %in% values) else metaData
    colnames(metaData)[which(colnames(metaData) == "filter_var")] <- variable
  }, error = function(e) {
    print(e)
    browser()
  })
  rownames(metaData) <- metaData$ID
  metaData <- metaData[-which(colnames(metaData) == "ID")]
  return(metaData)
}

fixSpecialCharacters <- function(x, offendingChars)
{
  for (char in offendingChars) {
    x <- str_replace(x, paste0("\\", char), "")
  }
  
  return(x)
}

