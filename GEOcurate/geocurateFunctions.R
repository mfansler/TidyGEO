library(rdrop2)
library(GEOquery)
library(stringr)
library(dplyr)
library(readr)
library(glue)



saveLines <- function(strings, oFile) {
  for (el in strings) {
    oFile <- c(oFile, el)
  }
  
  return(oFile)
}

saveToRscript <- function(oFile, filePath = file.path(tempdir(), "script_Temp.R")) {
  sink(filePath, append = FALSE, split = FALSE)
  for(i in 1:length(oFile)) cat(oFile[i], fill = T)
  sink()
}

removeFromScript <- function(oFile, len, all = F) {
  length(oFile) <- if(all) len else length(oFile) - len
  return(oFile)
}

saveRscript <- function() {
  token <- readRDS("droptoken.rds")
  
  filePath <- file.path(tempdir(), "script_Temp.R")
  drop_upload(filePath, path = "Shiny", dtoken = token)
}

saveData <- function(metaData, fileName) {
  token <- readRDS("droptoken.rds")
  
  filePath <- file.path(tempdir(), fileName)
  write.csv(metaData, filePath, row.names = FALSE)
  drop_upload(filePath, path = "Shiny", dtoken = token)
}

loadData <- function(geoID, downloadExpr = FALSE, downloadPlatform = FALSE) {
  token <- readRDS("droptoken.rds")
  
  currPath <- paste0("/Shiny/", geoID, "_Clinical_Raw.csv")
  expressionPath <- paste0("/Shiny/", geoID, "_Expression_Raw.csv")
  # Read all the files into a list
  filesInfo <- drop_dir("Shiny")
  #print(paste("filesInfo", filesInfo))
  filePaths <- filesInfo$path_display
  #print(paste("filePaths", filePaths))
  allData <- NULL
  if (currPath %in% filePaths) {
    print("File found")
    filePath <- filePaths[which(filePaths == currPath)]
    data <- lapply(filePath, drop_read_csv, stringsAsFactors = FALSE, row.names = "geo_accession", dtoken = token)
    ## Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    #print(head(as.data.frame(data)))
    allData[["metaData"]] <- as.data.frame(data)
  }
  if (downloadExpr) {
    if (expressionPath %in% filePaths) {
      print("Expression file found")
      filePath <- filePaths[which(filePaths == expressionPath)]
      exprData <- lapply(filePath, drop_read_csv, stringsAsFactors = FALSE, row.names = "probes", dtoken = token)
      ## Concatenate all data together into one data.frame
      exprData <- do.call(rbind, exprData)
      #print(head(as.data.frame(data)))
      allData[["expressionData"]] <- as.data.frame(exprData)
    }
  }

  return(allData)
}

downloadClinical <- function(geoID, toFilter, session = NULL, otherDownloads = NULL)
{
  dataSetIndex = 1
  
  downloadExpr <- if_else("downloadExpr" %in% otherDownloads, TRUE, FALSE)
  downloadPlatform <- if_else("downloadPlatform" %in% otherDownloads, TRUE, FALSE)
  
  if (grepl("_", geoID)) {
    parts <- str_split(geoID, "_")[[1]]
    geoID <- parts[1]
    subname <- parts[2]
    if (subname == "U133A") {
      dataSetIndex = 2
    }
  }
  
  #Download data
  status <- tryCatch({
    allData <- loadData(geoID, downloadExpr, downloadPlatform)
    
    incProgress(1/2)
    #print(head(metaData))
    
    if (is.null(allData)) {
      allData <- downloadData(geoID, dataSetIndex, downloadExpr, downloadPlatform)
    }
    "pass"
  }, error = function(e) {
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
  
  #metaData <- cbind(SampleID = rownames(metaData), metaData)
  
  if (status == "pass") {
    incProgress(3/4)
    allData[["metaData"]] <- filterUninformativeCols(allData[["metaData"]], toFilter)
    #print(head(metaData))

    
    return(allData)
  }
  
  else if (!is.null(session)){
    print(status)
    createAlert(session, "alert", "fileError", title = "Error",
                content = unlist(status), append = FALSE)
    
    return(NULL)
    
  }
}

downloadData <- function(geoID, dataSetIndex, downloadExpr = FALSE, downloadPlatform = FALSE) {
  #temppath <- file.path(tempdir(), "geo/series/")
  #dir.create(temppath, showWarnings = F)
  expressionSet <- getGEO(GEO = geoID, GSEMatrix = TRUE, getGPL = FALSE)
  
  
  if (dataSetIndex > length(expressionSet))
    stop(paste("The dataSetIndex value was ", dataSetIndex, " but there are only ", length(expressionSet), " objects in GEO.", sep=""))
  
  # Retrieve data values depending on the platform
  #if (length(expressionSet) > 1) idx <- grep("GPL6947", attr(expressionSet, "names")) else idx <- 1
  expressionSet <- expressionSet[[dataSetIndex]]
  
  # Extract meta data frame
  metaData <- pData(expressionSet)
  
  saveData(metaData, paste0(geoID, "_Clinical_Raw.csv"))
  
  if(downloadExpr) {
    expressionData <- assayData(expressionSet)$exprs
    saveData(cbind("probes" = rownames(expressionData), expressionData), paste0(geoID, "_Expression_Raw.csv"))
  } else {
    expressionData <- NULL
  }
  #myData <- loadData(geoID)
  
  allData <- list("metaData" = metaData, "expressionData" = expressionData)
                     
  return(allData)
}

#filter columns with all different entries or all the same entry
filterUninformativeCols <- function(metaData, toFilter = list("none"))
{
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
      isReanalyzed <- if("reanalyzed" %in% toFilter) grepl("Reanaly[sz]ed ", temp) else FALSE
      isURL <- if("url" %in% toFilter) grepl("ftp:\\/\\/", temp) else FALSE
      isDate <- if("dates" %in% toFilter) grepl("[A-Za-z]+ [0-9]{1,2},? [0-9]{2,4}", temp) else FALSE
      
      if(all(grepl("[A-Za-z]+ [0-9]{1,2},? [0-9]{2,4}", temp))) {
        
        uniqueDates <- unique(temp)
        dateCounts <- NULL
        for (uniqueDate in uniqueDates) {
          #count the number of rows that have this date and store it in a list
          dateCounts[[uniqueDate]] <- length(which(temp == uniqueDate))
          #print(dateCounts)
        }
        
        m <- regexpr("[A-Za-z]+ [0-9]{1,2},? [0-9]{2,4}", metaData[1,i], perl=TRUE)
        prev <- regmatches(metaData[1,i], m)
        
        for (j in 1:length(metaData[,i])) {
          
          #if(metaData[j,i] == "NA" || metaData[j,i] == "") {
          #  evalSame[j] <- "NA"
          #}
          
          #else {
            #extract the actual date part
            m <- regexpr("[A-Za-z]+ [0-9]{1,2},? [0-9]{2,4}", metaData[j,i], perl=TRUE)
            myDate <- regmatches(metaData[j,i], m)
            
            #the date is not the same as the previous one and the current date and the previous date represent a substantial amount of the entries
            if (myDate != prev && (dateCounts[[metaData[j,i]]] > (nrow(metaData)/3)) && (dateCounts[[metaData[j-1,i]]] > (nrow(metaData)/3))) {
              evalSame[j] <- 1
            }
            prev <- myDate
          #}
        }
      }
      
      uniqueVals <- unique(as.factor(as.character(toupper(temp))))
      notAllSame <- if("sameVals" %in% toFilter) length(uniqueVals) > 1 else TRUE
      notAllDifferent <- if("allDiff" %in% toFilter) length(uniqueVals) != length(rownames(metaData)) else TRUE
      
      if(notAllSame && notAllDifferent && !all(isReanalyzed) && !all(isURL) && !all(isDate) && metaData[i] != rownames(metaData)) {
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
  
  #check if any columns are duplicates
  for (column in colnames(filteredData)) {
    columns <- colnames(filteredData)[-which(colnames(filteredData) == column)]
    for (thisCol in columns) {
      isIdentical <- identical(filteredData[[column]], filteredData[[thisCol]])
      if (isIdentical) {
        filteredData <- filteredData[,which(colnames(filteredData) != thisCol)]
      }
    }
  }
  if (!("evalSame" %in% colnames(metaData))) {
    filteredData <- cbind(filteredData, evalSame)
  }
  return(filteredData)
}

isAllNum <- function(metaData) {
  #print(metaData[,1])
  vals <- unique(metaData[,1])
  temp <- suppressWarnings(as.numeric(as.character(metaData[,1])))
  isNum <- all(is.numeric(temp)) && all(!is.na(temp)) && length(vals) > 2
  return(isNum)
}

isAllUnique <- function(metaData) {
  vals <- unique(metaData[,1])
  return (length(vals) == nrow(metaData))
}

printVarsSummary <- function(metaData) {
  summFrame <- data.frame(a="", b="", stringsAsFactors = FALSE)
  #print(colnames(metaData))
  for (variable in colnames(metaData)) {
    if(variable == "evalSame")
      next
    varName <- c(paste0("Summary for variable \"", variable, "\":"), "")
    summFrame <- rbind(summFrame, varName)
    
    naCount <- sum(is.na(metaData[,variable]))
    temp <- as.numeric(as.character(metaData[,variable]))
    isNum <- all(is.numeric(temp)) && all(!is.na(temp))
    #print(metaData[,variable])
    #print(isNum)
    if (all(isNum)) {
      metaData[,variable] <- as.numeric(as.character(metaData[,variable]))
      descript <- paste("Values range from", min(metaData[,variable]),
                        "to", max(metaData[,variable]), sep = " ")
      NAdescript <- "NA Count"
      summFrame <- rbind(summFrame, c(descript, NAdescript))
      summFrame <- rbind(summFrame, c("", naCount))
    }
    else {
      uniqueVals <- unique(metaData[,variable])
      #print(uniqueVals)
      if (length(uniqueVals) == length(rownames(metaData))) {
        descript <- "All values are unique.\nThis variable might not be informative."
        NAdescript <- "NA Count"
        summFrame <- rbind(summFrame, c(descript, NAdescript))
        summFrame <- rbind(summFrame, c("", naCount))
      }
      else {
        descript <- c("Unique values", "NA Count")
        summFrame <- rbind(summFrame, descript)
        #print(as.character(uniqueVals[[1]]))
        descript <- c(as.character(uniqueVals[1]), naCount)
        summFrame <- rbind(summFrame, descript)
        for (i in 2:length(uniqueVals)) {
          summFrame <- rbind(summFrame, c(as.character(uniqueVals[i]), ""))
        }
      }
    }
    summFrame <- rbind(summFrame, c("", ""))
  }
  return(summFrame)
}


createExampleCols <- function(metaData, colsToDivide, delimiter) {
  for (col in colsToDivide) {
    divideExamples[[col]] <- str_split(metaData[1, col], delimiter)[[1]]
  }
  return(divideExamples)
}


saveFileDescription <- function(geoID, filePathToSave) {
  
  desFilePath <- paste(filePathToSave, "_Description.md", sep = "")
  if (!file.exists(desFilePath)) {  
    url <- paste("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", geoID, "&targ=self&form=text&view=quick", sep = "")
    tempFile <- paste("Clinical_Raw/", geoID, "__ncbi.txt", sep= "")
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
        if(term == "!Series_pubmed_id") {
          value <- paste("[PubMed article]", "(https://www.ncbi.nlm.nih.gov/pubmed/", value, ")", sep = "")
        }
      }
      valuesList <- append(valuesList, value)
    }
    
    if (!dir.exists(dirname(desFilePath)))
      dir.create(dirname(desFilePath), recursive = TRUE)
    
    for(i in 1:length(titles)) {
      write(c(titles[i], valuesList[i]), file = desFilePath, append = TRUE, sep = "\n")
      write("", file = desFilePath, append = TRUE, sep = "\n")
    }
  }
}



extractColNames <- function(inputDataFrame, delimiterInfo)
{
  incProgress()
  classAndClinical <- inputDataFrame
  prefixes = NULL
  
  for (i in 1:nrow(classAndClinical))
  {
    for (j in 1:ncol(classAndClinical))
    {
      value <- as.character(classAndClinical[i,j])
      pattern <- delimiterInfo[which(delimiterInfo == colnames(classAndClinical[j]))+1]
      
      if (colnames(classAndClinical)[j] %in% delimiterInfo && grepl(pattern, value)) {

        prefix <- str_split(value, pattern)[[1]][1]
        prefix <- str_trim(prefix)

      }
      else {
        prefix <- colnames(classAndClinical)[j]
      }
      
      if (prefix != "")
        prefixes <- c(prefixes, prefix)
    }
    incProgress()
  }
  
  prefixes <- sort(unique(prefixes))
  
  classAndClinical2 <- data.frame(rownames(classAndClinical))
  
  for (prefix in prefixes)
  {
    rowValues = NULL
    
    for (i in 1:nrow(classAndClinical))
    {
      rowValue = "?"
      
      for (j in 1:ncol(classAndClinical))
      {
        value <- as.character(classAndClinical[i,j])
        pattern <- delimiterInfo[which(delimiterInfo == colnames(classAndClinical[j]))+1]
        
        if (colnames(classAndClinical)[j] %in% delimiterInfo && grepl(pattern, value)) {
          valueParts <- str_split(value, pattern)[[1]]
          thisPrefix <- str_trim(valueParts[1])
          thisValue <- valueParts[2]
        }
        
        else
        {
          thisPrefix <- colnames(classAndClinical)[j]
          thisValue <- value
        }
        
        if (thisPrefix == prefix) {
          thisValue = str_trim(thisValue)
          rowValue = thisValue
        }
      }
      
      rowValues <- c(rowValues, rowValue)
    }
    
    classAndClinical2 <- cbind(classAndClinical2, rowValues)
    
    incProgress()
  }
  
  prefixesNew <- c("SampleID", str_replace_all(prefixes, " ", "_"))
  prefixesNew <- str_replace_all(prefixesNew,"\\(","")
  prefixesNew <- str_replace_all(prefixesNew,"\\)","")
  
  colnames(classAndClinical2) <- prefixesNew
  
  rownames(classAndClinical2) <- as.vector(classAndClinical2[,1])
  classAndClinical2 <- classAndClinical2[,-1]
  
  classAndClinical2 <- filterUninformativeCols(classAndClinical2, list("none"))
  
  return(classAndClinical2)
}

splitCombinedVars <- function(metaData, colsToDivide, delimiter, numElements)
{
  incProgress()
  
  #print(numElements)
  targetCols <- colsToDivide
  for (colName in targetCols) {
    targetCol <- metaData[,colName]
    if (numElements[[colName]] > 1) {
      colNames <- NULL
      #print(numElements[[colName]])
      for (i in 1:numElements[[colName]]) {
        colNames <- c(colNames, paste(colName, i, sep = "."))
      }
      #print(colNames)
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
    incProgress()
  }
  
  metaData <- filterUninformativeCols(metaData, list("none"))
  
  return(metaData)
}

extractCols <- function(metaData, toSplit, colsToSplit, toDivide, colsToDivide, delimiter, delimiter2, allButSplit, allButDivide) {
  
  if(toSplit && (!is.null(colsToSplit) || allButSplit) && delimiter != "" && !is.null(metaData)) {
    delimiterInfo <- NULL
    #print(colsToSplit)
    if (allButSplit) {
      colsToSplit <- if (is.null(colsToSplit)) colnames(metaData) else colnames(metaData[-which(colnames(metaData) %in% colsToSplit)])
      colsToSplit <- colsToSplit[-which(colsToSplit == "evalSame")]
    }
    for (col in colsToSplit) {
      delimiterInfo <- c(delimiterInfo, col, delimiter)
    }
    #print(delimiterInfo)
    metaData <- extractColNames(metaData, delimiterInfo)
    
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

    metaData <- splitCombinedVars(metaData, colsToDivide, delimiter2, numElements)
  }
  
  for (col in colnames(metaData)) {
    metaData[,col] <- sapply(metaData[,col], function(x){gsub(x, pattern = "NA", replacement = NA)})
  }
  
  return(metaData)
}

filterCols <- function(metaData, varsToKeep, allButKeep) {
  
  if (allButKeep) {
    
    metaData <- if(!is.null(varsToKeep)) metaData[-which(colnames(metaData) %in% varsToKeep)] else metaData

  }
  else {
    varsToKeep <- c(varsToKeep, "evalSame")
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

renameCols <- function(metaData, newNames, session) {
  #print(colsToRename)
  #print(newNames)
  updatedCols <- NULL
  for (colName in colnames(metaData)) {
    if (colName %in% names(newNames)) {
      offendingChars <- findOffendingChars(newNames[colName])
      if(any(offendingChars)) {
        createAlert(session, "alert", "offendingChars",
                    content = paste("The following characters were removed from", 
                                    newNames[colName], 
                                    "because they might cause problems later:", 
                                    collapse(names(offendingChars[which(offendingChars == T)]), sep = ", ")))
        #newNames[colName] <- fixSpecialCharacters(newNames[colName], names(offendingChars[which(offendingChars == T)]))
      }
      updatedCols <- c(updatedCols, newNames[colName])
    }
    else {
      updatedCols <- c(updatedCols, colName)
    }
  }
  #print(updatedCols)
  updatedCols <- make.names(updatedCols)
  colnames(metaData) <- updatedCols
  return(metaData)
  
  
}


substituteVals <- function(classAndClinical, subSpecs)
{
  #print(subSpecs)
  for(colToSub in names(subSpecs)) {
    subs <- subSpecs[[colToSub]]
    #print(subs)
    toSub <- subs$To_Replace
    #print(toSub)
    newVal <- if (subs$New_Val == "NA" || subs$New_Val == "") NA else as.character(subs$New_Val)
    #print(newVal)
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


cleanValues <- function(data, specs, unknownVal = "?")
{
  names <- colnames(data)
  names2 <- NULL
  for (name in names)
  {
    names2 <- c(names2, fixSpecialCharacters(name))
  }
  colnames(data) <- names2
  
  if (FALSE) {
  for (variable in specs$Name) {
    NAVals <- as.character(specs[which(specs$Name == variable), "NA Vals"])
    NAVals <- str_replace_all(NAVals, "([\\[\\]\\'])", "")
    NAVals <- str_split(NAVals, ", ")[[1]]
    for (j in 1:nrow(data)) {
      if (toupper(as.character(data[j,variable])) %in% c("NA","UNKNOWN", "NOT KNOWN", "N/A","MISSING", "--", "NON CASE", "NOT AVAILABLE", "_", "NONE","?")) {
        data[j,variable] <- unknownVal
      }
      if (as.character(data[j,variable]) %in% NAVals) {
        data[j,variable] <- unknownVal
      }
    }
  }
  }


  
  return(data)
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
      for(el in toExclude[which(!toExclude %in% metaData[,variable])]) {
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


getClinicalData <- function(geoID, rawFilePath, outputRawFilePath, specs, subname = "")
{

  
  #geoID = "GSE10320"
  #subname = ""
  #titledValues = F
  #colNames = "Relapse"
  #columnFilter="characteristics"
  #replicatedData = F
  
  
  metaData <- downloadClinical(rawFilePath)
  metaData <- metaData[specs$Name]
    
  # Fix column names
  colNames <- NULL
  for (varName in colnames(metaData)){
    preferredName <- specs$PreferredName[which(specs$Name == varName)]
    if (!is.na(preferredName)){
      colNames <- c(colNames, specs$PreferredName[which(specs$Name == varName)])
    }
    else {
      colNames <- c(colNames, varName)
    }
  }
  colnames(metaData) <- colNames
  
  
  metaData <- cleanValues(metaData, specs, unknownVal = NA)
  
  metaData <- excludeVars(metaData, specs)

  subSpecs <- specs[which(!is.na(specs$Substitutions)),]
  metaData <- substituteVals(metaData, subSpecs)
  
  metaData <- cbind(SampleID = rownames(metaData), metaData)
    
  saveClinicalData(geoID, metaData, outputRawFilePath, TRUE) 
  
  
  return(metaData)
}
saveClinicalData <- function(geoID, metaData, outputRawFilePath, saveDescription = FALSE)
{

  print(paste("Saving clinical data to ", outputRawFilePath, sep=""))
    
  if (!dir.exists(dirname(outputRawFilePath)))
    dir.create(dirname(outputRawFilePath), recursive=TRUE, showWarnings=FALSE)
    
  write.table(metaData, outputRawFilePath, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
    
  if (saveDescription) 
    saveFileDescription(geoID, str_split(outputRawFilePath, "\\.")[[1]][1])


}