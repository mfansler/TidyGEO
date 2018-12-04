library(rdrop2)
library(GEOquery)
library(stringr)
library(dplyr)
library(readr)
library(glue)

saveLines <- function(strings, oFile) {
  
  oFile <- c(oFile, strings)
  
  return(oFile)
}

saveToRscript <- function(oFile, filePath = file.path(tempdir(), "script_Temp.R")) {
  sink(filePath, append = FALSE, split = FALSE)
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

downloadClinical <- function(geoID, toFilter, session = NULL, downloadExpr = FALSE) {
  
  expressionSet <- loadRdsFromDropbox(geoID)
  
  if (is.null(expressionSet)) {
    status <- tryCatch({
      if (!grepl("GSE", geoID)) {
        stop('Please enter an ID that begins with "GSE".', call. = FALSE)
      }
      expressionSet <- getGEO(GEO = geoID, GSEMatrix = TRUE, getGPL = TRUE, AnnotGPL = TRUE)
      saveDataRDS(expressionSet, paste0(geoID, ".rds"))
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
    if (status != "pass" && !is.null(session)) {
      createAlert(session, "alert", "fileError", title = "Error",
                  content = unlist(status), append = FALSE)
    }
  }
  
  return(expressionSet)
}

processData <- function(expressionSet, index, toFilter, extractExprData = FALSE, session = NULL) {
  
  expressionSet <- expressionSet[[index]]
  
  if (extractExprData) {
    
    incProgress(message = "Extracting expression data")
    expressionData <- assayData(expressionSet)$exprs
    #expressionData <- data.frame("ID" = rownames(expressionData), expressionData)
    expressionData <- data.frame("ID" = rownames(expressionData), apply(expressionData, 2, as.numeric))

    incProgress(message = "Extracting feature data")
    featureData <- data.frame(fData(expressionSet), stringsAsFactors = TRUE)
    
    hasNA <- as.logical(apply(featureData, 2, function(x) 
    {
      return(any(is.na(x)) || any(x == ""))
    }
    ))
    if (any(hasNA)) {
      featureData <- featureData[, -which(hasNA)]
    }
    
    if (nrow(expressionData) == 0) {
      expressionData <- NULL
    }
    if (nrow(featureData) == 0) {
      featureData <- NULL
    }
    metaData <- NULL
    
  } else {
    
    incProgress(message = "Extracting metadata")
    metaData <- as.data.frame(pData(expressionSet), stringsAsFactors = FALSE)
    incProgress(message = "Filtering metadata.")
    filtered_data <- filterUninformativeCols(metaData, toFilter)
    if (is.null(filtered_data)) {
      if (!is.null(session)) {
        createAlert(session, "alert", "parseError", title = "No columns removed",
                    content = "The specified column filters would have removed all the columns.")
      }
    } else {
      metaData <- filtered_data
    }
    
    #metaData <- cbind(metaData, evalSame = rep(1, nrow(metaData)))
    
    expressionData <- NULL
    featureData <- NULL
    
  }
  
  return(list("metaData" = metaData, "expressionData" = expressionData, "featureData" = featureData))
}

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
  
  cols_to_keep <- apply(metaData, 2, evaluate_cols_to_keep, toFilter = toFilter)
  
  if (all(!cols_to_keep)) {
    print("No informative columns found.")
    NULL
  } else {
    metaData <- metaData[,cols_to_keep]
  }
  metaData
}

isAllNum <- function(metaData) {
  toEvaluate <- metaData[,1][which(!is.na(metaData[,1]))]
  vals <- unique(toEvaluate)
  temp <- suppressWarnings(as.numeric(as.character(toEvaluate)))
  isNum <- all(is.numeric(temp)) && all(!is.na(temp)) && length(vals) > 2
  return(isNum)
}

isAllUnique <- function(metaData) {
  vals <- unique(metaData[,1])
  return(length(vals) == nrow(metaData))
}

printVarsSummary <- function(metaData) {
  summFrame <- data.frame(a = "", b = "", stringsAsFactors = FALSE)
  for (variable in colnames(metaData)) {
    #if (variable == "evalSame")
    #  next
    varName <- c(paste0("Summary for variable \"", variable, "\":"), "")
    summFrame <- rbind(summFrame, varName)
    
    naCount <- sum(is.na(metaData[,variable]))
    temp <- as.numeric(as.character(metaData[,variable]))
    isNum <- all(is.numeric(temp)) && all(!is.na(temp))
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
      if (length(uniqueVals) == length(rownames(metaData))) {
        descript <- "All values are unique.\nThis variable might not be informative."
        NAdescript <- "NA Count"
        summFrame <- rbind(summFrame, c(descript, NAdescript))
        summFrame <- rbind(summFrame, c("", naCount))
      }
      else {
        descript <- c("Unique values", "NA Count")
        summFrame <- rbind(summFrame, descript)
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

extractColNames <- function(inputDataFrame, delimiter, colsToSplit) {
  
  errorMessage <- NULL
  
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
      
      errorMessage <- c(errorMessage, paste0(col, " could not be split."),
                        paste0("Rows: ", offendingRows),
                        paste0("Values: ", offendingVals))
    }
  }
  
  if (!is.null(errorMessage)) {
    errorMessage <- c(paste0('<b>Looks like there are some cells that don\'t contain the delimiter "', delimiter, '".</b>'),
                      errorMessage)
    errorMessage <- paste(errorMessage, collapse = "<br/>")
    errorMessage <- paste('<font color="red">', errorMessage, '</font>')
    showModal(
      modalDialog(HTML(errorMessage), title = "Error", footer = modalButton("OK")
      )
      )
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
      #colsToSplit <- colsToSplit[-which(colsToSplit == "evalSame")]
    }
    
    metaData <- extractColNames(metaData, delimiter, colsToSplit)
    
  }
  
  if (toDivide && (!is.null(colsToDivide) || allButDivide) && delimiter2 != "" && !is.null(metaData)) {
    numElements <- NULL
    
    if (allButDivide) {
      colsToDivide <- if (is.null(colsToDivide)) colnames(metaData) else colnames(metaData[-which(colnames(metaData) %in% colsToDivide)])
      #colsToDivide <- colsToDivide[-which(colsToDivide == "evalSame")]
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
    #varsToKeep <- c(varsToKeep, "evalSame")
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
  
  if (names(newNames) %in% colnames(metaData)) {
    #offendingChars <- findOffendingChars(unname(newNames))
    #if (any(offendingChars)) {
    #  createAlert(session, "alert", "offendingChars",
    #              content = paste("The following characters were removed from", 
    #                              newNames[colName], 
    #                              "because they might cause problems later:", 
    #                              paste(names(offendingChars[which(offendingChars == T)]), collapse = ", ")))
    #newNames[1] <- str_replace_all(unname(newNames), "[^\._\s0-9A-Za-z]", ".")
    #}
    colnames(metaData)[which(colnames(metaData) == names(newNames))] <- unname(newNames)
  }
  return(metaData)
}

substitute_vals <- function(clinical_data, sub_specs, isnt_reg_ex = FALSE)
{
  col_to_sub <- names(sub_specs) 
  subs <- sub_specs[[col_to_sub]]
  
  if (any(subs$New_Val == "NA" | subs$New_Val == "")) {
    subs$New_Val[which(subs$New_Val == "NA" | subs$New_Val == "")] <- NA
  }
  
  for (i in 1:nrow(subs)) {
    if (grepl("RANGE", subs$To_Replace[i])) {
      
      mySub <- str_remove(subs$To_Replace[i], "RANGE: ")
      mySub <- as.numeric(str_split(mySub, "-")[[1]])
      
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
  return(clinical_data)
}

excludeVars <- function(metaData, specs) {
  for (variable in names(specs)) {
    toExclude <- specs[[variable]]
    if (any(toExclude == "NA") && any(is.na(metaData[,variable]))) {
      toExclude <- toExclude[-which(toExclude == "NA")]
      metaData <- metaData[-which(is.na(metaData[,variable])),]
    }
    if (!identical(toExclude, character(0))) {
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
      toExclude <- toExclude[which(toExclude %in% metaData[,variable])]
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

quickTranspose <- function(dataToTranspose) {
  
  incProgress()
  if (any(duplicated(dataToTranspose$ID))) {
    transposed <- dataToTranspose
    
    showNotification("Data cannot be transposed because the ID column is not all unique. Please specify a summarize option.")
    
  } else {
    transposed <- dataToTranspose %>%
      gather(newrows, valname, -ID) %>%
      spread(ID, valname) %>%
      dplyr::rename(ID = "newrows")
  }
  
  incProgress()
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
  
  incProgress()
  
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
  
  incProgress()
    
  return(mergedData)
}

findExprLabelColumns <- function(ftData) {
  allDiff <- as.logical(lapply(ftData, function(x){
    length(unique(x)) == nrow(ftData)
  }))
  return(colnames(ftData[allDiff]))
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

advance_columns_view <- function(data, start, forward_distance) {
  
  if (class(start) == "character") {
    start <- which(colnames(data) == start) + 1
  }
  
  end_point <- start + (forward_distance - 1)
  
  if (end_point > ncol(data)) {
    end_point <- ncol(data)
  }
  
  if (start >= end_point ) {
    return(NULL)
  }
  
  data[,start:end_point]
  
}

retract_columns_view <- function(data, last_column, backward_distance) {
  
  if (class(last_column) == "character") {
    last_column <- which(colnames(data) == last_column) - 1
  }
  
  start_point <- last_column - (backward_distance - 1)
  
  if (start_point < 1) {
    start_point <- 1
  }
  
  if (start_point > last_column) {
    return(NULL)
  }
  
  data[,start_point:last_column]
}

#finds all of data1 in data2
find_intersection <- function(data1, data2, id_col1 = "ID", id_col2 = "ID") {
  
  search_terms <- if (id_col2 == "colnames") c(colnames(data2)) else data2[,id_col2]
  
  if (id_col1 == "colnames") {
    data1[,which(colnames(data1) %in% c(search_terms, "ID"))]
  } else {
    data1[which(data1[,id_col1] %in% search_terms),]
  }
}