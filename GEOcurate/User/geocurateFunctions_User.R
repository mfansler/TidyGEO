library(GEOquery)
library(stringr)
library(glue)
library(dplyr)

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
  expressionSet <- getGEO(GEO = geoID, GSEMatrix = TRUE, getGPL = FALSE)
  
  expressionSet <- expressionSet[[dataSetIndex]]
  
  # Extract meta data frame
  metaData <- pData(expressionSet)
                     
  return(metaData)
}

downloadExpression <- function(geoID, dataSetIndex) {
  expressionSet <- getGEO(GEO = geoID, GSEMatrix = TRUE)
  
  expressionSet <- expressionSet[[dataSetIndex]]
  
  
  expressionData <- assayData(expressionSet)$exprs
  expressionData <- data.frame("ID" = rownames(expressionData), apply(expressionData, 2, as.numeric))
  
  
  featureData <- as.data.frame(fData(expressionSet), stringsAsFactors = FALSE)
  hasNA <- as.logical(apply(featureData, 2, function(x) any(is.na(x))))
  if (all(hasNA) != FALSE) {
    featureData <- featureData[, -which(hasNA)]
  }
  
  return(list("expressionData" = expressionData, "featureData" = featureData))
}

#filter columns with all different entries or all the same entry
filterUninformativeCols <- function(metaData, toFilter = list("none"))
{
  #filter out duplicates
  metaData <- metaData[!duplicated(as.list(metaData))]
  
  filteredData <- as.data.frame(row.names(metaData))
  dataToFilter <- matrix(nrow = nrow(metaData), ncol = 1)
  
  metaDataCols <- colnames(metaData)
  colNames <- NULL
  unFilteredCount = 0
  evalSame <- rep(0, nrow(metaData))
  
  for (i in 1:ncol(metaData)) {
    colName <- as.character(colnames(metaData)[i])

    temp <- metaData[,i]
    temp <- temp[which(temp != "NA")]
    temp <- temp[which(temp != "")]
    if (length(temp) > 0) {
      isReanalyzed <- if ("reanalyzed" %in% toFilter) grepl("Reanaly[sz]ed ", temp) else FALSE
      isURL <- if ("url" %in% toFilter) grepl("ftp:\\/\\/", temp) else FALSE
      isDate <- if ("dates" %in% toFilter) grepl("[A-Za-z]+ [0-9]{1,2},? [0-9]{2,4}", temp) else FALSE
      isTooLong <- if ("tooLong" %in% toFilter) as.logical(lapply(temp, function(x) nchar(x) > 100)) else FALSE
      
      uniqueVals <- unique(as.factor(as.character(toupper(temp))))
      notAllSame <- if ("sameVals" %in% toFilter) length(uniqueVals) > 1 else TRUE
      notAllDifferent <- if ("allDiff" %in% toFilter) length(uniqueVals) != length(rownames(metaData)) else TRUE
      
      if (notAllSame && notAllDifferent && !all(isReanalyzed) && !all(isURL) && !all(isDate) && 
         !all(isTooLong) && metaData[i] != rownames(metaData)) {
        filteredData <- cbind(filteredData, metaData[,i])
        colNames <- c(colNames, colName)
        unFilteredCount <- unFilteredCount + 1
      }
    }
  }
  if (unFilteredCount == 0) {
    print("No informative columns found.")
  }
  filteredData <- as.data.frame(filteredData[,2:ncol(filteredData)])
  row.names(filteredData) <- row.names(metaData)
  colnames(filteredData) <- colNames
  
  return(filteredData)
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
  
  for (col in colsToSplit) {
    
    hasDelim <- as.logical(sapply(inputDataFrame[which(!is.na(inputDataFrame[,col])), col], function(x){
      str_detect(x, delimiter)
    }))
    if (all(hasDelim)) {
      inputDataFrame <- separate(inputDataFrame, col, sep = delimiter, into = c("key", "value"))
      inputDataFrame <- spread(inputDataFrame, key = "key", value = "value")
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
  
  return(inputDataFrame)
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

renameCols <- function(metaData, newNames) {
  updatedCols <- NULL
  for (colName in colnames(metaData)) {
    if (colName %in% names(newNames)) {
      updatedCols <- c(updatedCols, newNames[colName])
    }
    else {
      updatedCols <- c(updatedCols, colName)
    }
  }
  updatedCols <- make.names(updatedCols)
  colnames(metaData) <- updatedCols
  
  return(metaData)
}


substituteVals <- function(classAndClinical, subSpecs)
{
  for (colToSub in names(subSpecs)) {
    subs <- subSpecs[[colToSub]]
    toSub <- subs$To_Replace
    newVal <- if (subs$New_Val == "NA" || subs$New_Val == "") NA else as.character(subs$New_Val)
    for (i in 1:length(toSub)) {
      if (grepl("RANGE", toSub[i])) {
        mySub <- str_split(toSub[i], "RANGE: ")[[1]][2]
        mySub <- str_split(mySub, "-")[[1]]
        classAndClinical[,colToSub] <- sapply(classAndClinical[,colToSub], 
                                              function(x){
                                                if (mySub[1] <= x && x <= mySub[2]) newVal[i] else x
                                              })
      } else {
        classAndClinical[,colToSub] <- sapply(classAndClinical[,colToSub], 
                                              function(x){gsub(x, pattern = toSub[i], replacement = newVal[i])})
      }
    }
  }
  
  return(classAndClinical)
}

excludeVars <- function(metaData, specs) {
  for (variable in names(specs)) {
    toExclude <- specs[[variable]]
    if (any(toExclude == "NA") && any(is.na(metaData[,variable]))) {
      toExclude <- toExclude[-which(toExclude == "NA")]
      metaData <- metaData[-which(is.na(metaData[,variable])),]
      #take the nas out of toExclude
      #exclude all the stuff in toExclude
      #exclude the nas in the column
    }
    if (!identical(toExclude, character(0))) {
      #print(toExclude[which(!toExclude %in% metaData[,variable])])
      for (el in toExclude[which(!toExclude %in% metaData[,variable])]) {
        if (grepl("exclude", el)) {
          el <- str_split(el, "exclude: ")[[1]][2]
          bounds <- as.numeric(str_split(el, "-")[[1]])
          
          indices <- sapply(metaData[,variable], 
                            function(x){
                              if (bounds[1] <= x && x <= bounds[2]) FALSE else TRUE
                            })
          metaData <- metaData[which(indices),]
        }
        else if (grepl("keep", el)) {
          el <- str_split(el, "keep: ")[[1]][2]
          bounds <- str_split(el, "-")[[1]]
          metaData <- metaData[which(as.numeric(metaData[,variable]) >= bounds[1]),]
          metaData <- metaData[which(as.numeric(metaData[,variable]) <= bounds[2]),]
        }
        metaData[,variable] <- as.numeric(metaData[,variable])
      }
      #print(metaData)
      toExclude <- toExclude[which(toExclude %in% metaData[,variable])]
      #print(toExclude)
      metaData <- if (!identical(toExclude, character(0))) metaData[which(!(metaData[,variable] %in% toExclude)),] else metaData
    }
  }
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
    select(ID, replace)
  
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