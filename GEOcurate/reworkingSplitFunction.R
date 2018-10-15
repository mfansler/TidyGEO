library(tidyverse)
GSE1456_Clinical_Raw <- read_delim("GEOcurate/User/GSE1456_Clinical_Raw.txt",
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
GSE1456 <- GSE1456_Clinical_Raw %>% 
  select(SampleID, characteristics_ch1.1, characteristics_ch1.2, characteristics_ch1.3, relation)
GSE1456 <- as.data.frame(GSE1456)

extractCols_test <- function(metaData, toSplit, colsToSplit, toDivide, colsToDivide, delimiter, delimiter2, allButSplit, allButDivide) {
  
  if(toSplit && (!is.null(colsToSplit) || allButSplit) && delimiter != "" && !is.null(metaData)) {
    delimiterInfo <- NULL
    if (allButSplit) {
      colsToSplit <- if (is.null(colsToSplit)) colnames(metaData) else colnames(metaData[-which(colnames(metaData) %in% colsToSplit)])
      colsToSplit <- colsToSplit[-which(colsToSplit == "evalSame")]
    }
    
    metaData <- extractColNames(metaData, delimiter, colsToSplit)
    
  }
  
  if(toDivide && (!is.null(colsToDivide) || allButDivide) && delimiter2 != "" && !is.null(metaData)) {
    numElements <- NULL
    
    if (allButDivide) {
      colsToDivide <- if (is.null(colsToDivide)) colnames(metaData) else colnames(metaData[-which(colnames(metaData) %in% colsToDivide)])
      colsToDivide <- colsToDivide[-which(colsToDivide == "evalSame")]
    }
    
    for (col in colsToDivide) {
      numElements[[col]] <- length(str_split(metaData[1, col], delimiter2)[[1]])
    }
    
    metaData <- splitCombinedVars_test(metaData, colsToDivide, delimiter2, numElements)
  }
  
  for (col in colnames(metaData)) {
    metaData[,col] <- sapply(metaData[,col], function(x){gsub(x, pattern = "NA", replacement = NA)})
  }
  
  return(metaData)
}

extractCols2 <- function(metaData, toSplit, colsToSplit, toDivide, colsToDivide, delimiter, delimiter2, allButSplit, allButDivide) {
  
  if(toSplit && (!is.null(colsToSplit) || allButSplit) && delimiter != "" && !is.null(metaData)) {
    delimiterInfo <- NULL
    if (allButSplit) {
      colsToSplit <- if (is.null(colsToSplit)) colnames(metaData) else colnames(metaData[-which(colnames(metaData) %in% colsToSplit)])
      colsToSplit <- colsToSplit[-which(colsToSplit == "evalSame")]
    }
    
    metaData <- extractColNames(metaData, delimiter, colsToSplit)
    
  }
  
  if(toDivide && (!is.null(colsToDivide) || allButDivide) && delimiter2 != "" && !is.null(metaData)) {
    numElements <- NULL
    
    if (allButDivide) {
      colsToDivide <- if (is.null(colsToDivide)) colnames(metaData) else colnames(metaData[-which(colnames(metaData) %in% colsToDivide)])
      colsToDivide <- colsToDivide[-which(colsToDivide == "evalSame")]
    }
    
    for (col in colsToDivide) {
      numElements[[col]] <- length(str_split(metaData[1, col], delimiter2)[[1]])
    }
    
    metaData <- splitCombinedVars2(metaData, colsToDivide, delimiter2, numElements)
  }
  
  for (col in colnames(metaData)) {
    metaData[,col] <- sapply(metaData[,col], function(x){gsub(x, pattern = "NA", replacement = NA)})
  }
  
  return(metaData)
}
#time these
oldSplit <- extractCols_test(GSE1456, FALSE, NULL, TRUE, c("characteristics_ch1.3"), NULL, ": ", FALSE, FALSE)
newSplit <- extractCols2(GSE1456, FALSE, NULL, TRUE, c("characteristics_ch1.3"), NULL, ": ", FALSE, FALSE)

splitCombinedVars2 <- function(metaData, colsToDivide, delimiter, numElements) {
  targetCols <- colsToDivide
  for (colName in targetCols) {
    targetCol <- metaData[,colName]
    if(numElements[[colName]] > 1) {
      colNames <- NULL
      for (i in 1:numElements[[colName]]) {
        colNames <- c(colNames, paste(colName, i, sep = "."))
      }
      metaData <- separate(metaData, col = colName, into = colNames, sep = delimiter)
    }
  }
  #metaData <- filterUninformativeCols(metaData)
  return(metaData)
}


splitCombinedVars_test <- function(metaData, colsToDivide, delimiter, numElements) {
  
  targetCols <- colsToDivide
  for (colName in targetCols) {
    targetCol <- metaData[,colName]
    if (numElements[[colName]] > 1) {
      colNames <- NULL
      for (i in 1:numElements[[colName]]) {
        colNames <- c(colNames, paste(colName, i, sep = "."))
      }
      newData <- data.frame(matrix(nrow = length(targetCol), ncol = length(colNames)))
      colnames(newData) <- colNames
      for (index in 1:length(targetCol)) {
        delimiter <- as.character(delimiter)
        newVar <- as.character(targetCol[index])
        vars <- str_split(newVar, delimiter)[[1]]
        vars <- str_trim(vars)
        for (i in 1:length(vars)) {
          newData[index, colNames[i]] <- vars[i]
        }
      }
      metaData <- cbind(metaData, newData)
      metaData <- metaData[, which(colnames(metaData) != colName)]
    }
  }
  
  #metaData <- filterUninformativeCols(metaData, list("none"))
  
  return(metaData)
}