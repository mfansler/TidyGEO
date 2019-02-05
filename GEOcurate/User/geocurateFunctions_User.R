library(GEOquery)
library(stringr)
library(glue)
library(dplyr)
library(tidyr)

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

downloadClinical <- function(geoID, toFilter, dataSetIndex, fileName = NA)
{
  
  #Download data
  status <- tryCatch({
    fileName <- paste0(geoID, "_Clinical_Raw.txt")
    metaData <- loadData(geoID)
    if (is.null(metaData)) {
      metaData <- downloadData(geoID, dataSetIndex)
    }
    "pass"
  }, error = function(e) {
    if (grepl("open\\.connection", paste0(e))) {
      print(paste0("Trouble establishing connection to GEO. Please try again later."))
      }
    else if (grepl("file\\.exists", paste0(e))) {
      print(paste0("File not found. Please enter a valid ID."))
    }
    else {
      print(paste(e))
    }
    }
  )
  
 metaData <- filterUninformativeCols(metaData, toFilter)
 return(metaData)
}

downloadData <- function(geoID, dataSetIndex) {
  expressionSet <- getGEO(geoID, getGPL = "FALSE")
  
  expressionSet <- expressionSet[[dataSetIndex]]
  
  # Extract meta data frame
  metaData <- pData(expressionSet)
                     
  return(metaData)
}

downloadExpression <- function(geoID, dataSetIndex) {
  expressionSet <- getGEO(geoID, getGPL = FALSE)
  
  expressionSet <- expressionSet[[dataSetIndex]]
  
  
  expressionData <- assayData(expressionSet)$exprs
  expressionData <- data.frame("ID" = rownames(expressionData), apply(expressionData, 2, as.numeric))
  
  
  featureData <- as.data.frame(fData(expressionSet), stringsAsFactors = FALSE)
  #hasNA <- as.logical(apply(featureData, 2, function(x) any(is.na(x))))
  #if (all(hasNA) != FALSE) {
  #  featureData <- featureData[, -which(hasNA)]
  #}
  
  return(list("expressionData" = expressionData, "featureData" = featureData))
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

extractColNames <- function(inputDataFrame, delimiter, colsToSplit) {
  
  inputDataFrame <- cbind(row_names = rownames(inputDataFrame), inputDataFrame)
  
  for (col in colsToSplit) {
    
    hasDelim <- as.logical(sapply(inputDataFrame[which(!is.na(inputDataFrame[,col])), col], function(x){
      str_detect(x, delimiter)
    }))
    if (all(hasDelim)) {
      inputDataFrame <- separate(inputDataFrame, col, sep = delimiter, into = c("key", "value"))
      
      col_names <- colnames(inputDataFrame)
      col_names <- append(col_names, unique(inputDataFrame$key)[which(!is.na(unique(inputDataFrame$key)))], which(col_names == "key"))
      col_names <- col_names[-which(col_names %in% c("key", "value"))]
      
      inputDataFrame <- spread(inputDataFrame, key = "key", value = "value")
      inputDataFrame <- inputDataFrame[,col_names]
    } else {
      
      offendingRows <- paste((1:length(hasDelim))[!hasDelim], collapse = ", ")
      offendingRows <- if_else(nchar(offendingRows) > 50, paste0(substr(offendingRows, 1, 50), "... "), offendingRows)
      
      offendingVals <- paste(inputDataFrame[!hasDelim, col], collapse = ", ")
      offendingVals <- if_else(nchar(offendingVals) > 50, paste0(substr(offendingVals, 1, 50), "... "), offendingVals)
      
      print(paste0(col, " could not be split."))
      print(paste0("Rows: ", offendingRows))
      print(paste0("Values: ", offendingVals))
    }
  }
  
  return(data.frame(inputDataFrame[,-1], row.names = inputDataFrame$row_names, check.names = FALSE))
}

splitCombinedVars <- function(metaData, colsToDivide, delimiter, numElements) {
  targetCols <- colsToDivide
  for (colName in targetCols) {
    #targetCol <- metaData[,colName]
    if (numElements[[colName]] > 1) {
      colNames <- NULL
      for (i in 1:numElements[[colName]]) {
        colNames <- c(colNames, paste(colName, i, sep = "."))
      }
      metaData <- separate(metaData, col = colName, into = colNames, sep = delimiter)
    }
  }
  return(metaData)
}

reformat_columns <- function(metaData, toSplit, colsToSplit, toDivide, colsToDivide, delimiter, delimiter2, allButSplit, allButDivide) {
  
  if (toSplit && (!is.null(colsToSplit) || allButSplit) && delimiter != "" && !is.null(metaData)) {
    #delimiterInfo <- NULL
    if (allButSplit) {
      colsToSplit <- if (is.null(colsToSplit)) colnames(metaData) else colnames(metaData[-which(colnames(metaData) %in% colsToSplit)])
    }
    
    metaData <- extractColNames(metaData, delimiter, colsToSplit)
    
  }
  
  if (toDivide && (!is.null(colsToDivide) || allButDivide) && delimiter2 != "" && !is.null(metaData)) {
    numElements <- NULL
    
    if (allButDivide) {
      colsToDivide <- if (is.null(colsToDivide)) colnames(metaData) else colnames(metaData[-which(colnames(metaData) %in% colsToDivide)])
    }
    
    for (col in colsToDivide) {
      numElements[[col]] <- length(str_split(metaData[1, col], delimiter2)[[1]])
    }
    
    metaData <- splitCombinedVars(metaData, colsToDivide, delimiter2, numElements)
  }
  
  metaData <- as.data.frame(apply(metaData, 2, function(x) {
    gsub(x, pattern = "NA", replacement = NA)
  }))
  
  metaData <- filterUninformativeCols(metaData)
  
  return(metaData)
}

filterCols <- function(metaData, varsToKeep, allButKeep) {
  
  if (allButKeep) {
    
    metaData <- if (!is.null(varsToKeep)) metaData[-which(colnames(metaData) %in% varsToKeep)] else metaData

  }
  else {
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


substitute_vals <- function(clinical_data, sub_specs, isnt_reg_ex = FALSE)
{
  col_to_sub <- names(sub_specs) 
  subs <- sub_specs[[col_to_sub]]
  row_names <- rownames(clinical_data)
  
  if (any(subs$New_Val == "NA")) {
    subs$New_Val[which(subs$New_Val == "NA")] <- NA
  }
  
  for (i in 1:nrow(subs)) {
    if (grepl("RANGE", subs$To_Replace[i])) {
      
      mySub <- str_remove(subs$To_Replace[i], "RANGE: ")
      mySub <- as.numeric(str_split(mySub, " - ")[[1]])
      
      new_col <- suppressWarnings(as.numeric(clinical_data[,col_to_sub]))
      new_col[
        which(!is.na(new_col) & 
                mySub[1] <= new_col &
                new_col <= mySub[2])
        ] <- as.character(subs$New_Val[i])
      
      clinical_data[which(!is.na(new_col)), col_to_sub] <- new_col[which(!is.na(new_col))]
      
    } else {
      
      clinical_data[,col_to_sub] <- sapply(as.character(clinical_data[,col_to_sub]), 
                                           function(x){
                                             gsub(x, pattern = subs$To_Replace[i], replacement = subs$New_Val[i], fixed = isnt_reg_ex)
                                           })
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

saveClinicalData <- function(geoID, metaData, outputRawFilePath, saveDescription = FALSE)
{

  print(paste("Saving clinical data to ", outputRawFilePath, sep = ""))
    
  if (!dir.exists(dirname(outputRawFilePath)))
    dir.create(dirname(outputRawFilePath), recursive = TRUE, showWarnings = FALSE)
    
  write.table(metaData, outputRawFilePath, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
    
  if (saveDescription) 
    saveFileDescription(geoID, str_split(outputRawFilePath, "\\.")[[1]][1])


}

quickTranspose <- function(dataToTranspose) {
  
  if (any(duplicated(dataToTranspose$ID))) {
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

replaceID <- function(data, replacement, replaceCol, summaryOption) {
  
  if (replaceCol == "ID") {
    return(data)
  }
  
  dataWRowNames <- data
  
  replacementWRowNames <- replacement %>%
    dplyr::rename(replace = replaceCol) %>%
    select(ID, replace) %>%
    filter(!is.na(replace) & replace != "")
  
  mergedData <- inner_join(dataWRowNames, replacementWRowNames, by = "ID") %>%
    select(-ID) %>%
    dplyr::rename(ID = "replace") %>%
    select(ID, everything())
  
  if (!is.null(summaryOption)) {
    if (summaryOption == "mean") {
      mergedData <- mergedData %>% group_by(ID) %>%
        summarize_all(mean) %>%
        ungroup()
    } else if (summaryOption == "median") {
      mergedData <- mergedData %>% group_by(ID) %>%
        summarize_all(median) %>%
        ungroup()
    } else if (summaryOption == "max") {
      mergedData <- mergedData %>% group_by(ID) %>%
        summarize_all(max) %>%
        ungroup()
    } else if (summaryOption == "min") {
      mergedData <- mergedData %>% group_by(ID) %>%
        summarize_all(min) %>%
        ungroup()
    }
  }
  
  return(mergedData)
}

filterExpressionData <- function(data, shinyFilterSpecs) {
  
  for (i in 1:length(shinyFilterSpecs)) {
    if (shinyFilterSpecs[i] != "") {
      if (grepl("\"", shinyFilterSpecs[i])) {
        searchStrs <- shinyFilterSpecs[i] %>%
          str_remove_all("\"") %>%
          str_remove_all("\\[") %>%
          str_remove_all("\\]") %>%
          str_split(",")
        searchStrs <- searchStrs[[1]]
        data <- data %>% filter_at(i, any_vars(. %in% searchStrs))
      } else if (grepl(" ... ", shinyFilterSpecs[i])) {
        searchStrs <- as.numeric(str_split(shinyFilterSpecs[i], " ... ")[[1]])
        data <- data %>% filter_at(i, any_vars(. > searchStrs[1] && . < searchStrs[2]))
      } else {
        matches <- sapply(data[,i], function(x) {
          grepl(shinyFilterSpecs[i], x)
        })
        data <- data[matches,]
      }
    }
  }
  return(data)
}

find_intersection <- function(data1, data2, id_col1 = "ID", id_col2 = "ID") {
  
  search_terms <- if (id_col2 == "colnames") c(colnames(data2)) else data2[,id_col2]
  
  if (id_col1 == "colnames") {
    data1[,which(colnames(data1) %in% c(search_terms, "ID"))]
  } else {
    data1[which(data1[,id_col1] %in% search_terms),]
  }
}