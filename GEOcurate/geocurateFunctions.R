library(rdrop2)
library(readr)
library(GEOquery)
library(stringr)
library(dplyr)
library(tidyr)

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

saveToRscript <- function(oFile, filePath = file.path(tempdir(), "script_Temp.R")) {
  file.copy('User/geocurateFunctions_User.R', filePath, overwrite = TRUE)
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

process_clinical <- function(expressionSet, session = NULL) {
  
  incProgress(message = "Extracting data")
  metaData <- pData(expressionSet)
  if (nrow(metaData) == 1) {
    metaData <- as.data.frame(t(as.matrix(apply(metaData, 2, replace_blank_cells))), row.names = rownames(metaData), stringsAsFactors = FALSE)
  } else {
    metaData <- as.data.frame(apply(metaData, 2, replace_blank_cells), row.names = rownames(metaData), stringsAsFactors = FALSE)
  }
  incProgress(message = "Filtering data")
  filtered_data <- filterUninformativeCols(metaData)
  if (is.null(filtered_data)) {
    if (!is.null(session)) {
      createAlert(session, "alert", "parseError", title = "No columns removed",
                  content = "The specified column filters would have removed all the columns.")
    }
  } else {
    metaData <- filtered_data
  }
  
  return(metaData)
}

process_expression <- function(expressionSet, index, session = NULL) {
    
  incProgress(message = "Extracting expression data")
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
    createAlert(session, "alpha_alert", "parseError", title = "Warning",
                content = paste("Some of the data is non-numeric.",
                                "This data is not supported by our some of our functionality.",
                                "Feel free to download the data to edit with another application."), append = FALSE)
  }
  
  incProgress(message = "Extracting feature data")
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
      metaData <- metaData[cols_to_keep]
    }
  }
  metaData
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
  
  if (is.null(colsToSplit) || delimiter == "") {
    return(inputDataFrame) 
  }
  
  errorMessage <- NULL
  
  inputDataFrame <- cbind(row_names = rownames(inputDataFrame), inputDataFrame)
  
  for (col in colsToSplit) {
    
    hasDelim <- is.na(inputDataFrame[, col]) | str_detect(inputDataFrame[, col], delimiter)
    
    if (all(hasDelim)) {
      inputDataFrame <- separate(inputDataFrame, col, sep = delimiter, into = c("key", "value")) %>%
        mutate(key = str_trim(key), value = str_trim(value))
      
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
  
  metaData <- data.frame(inputDataFrame[,-1], row.names = inputDataFrame$row_names, check.names = FALSE)
  
  metaData <- as.data.frame(apply(metaData, 2, function(x) {
    gsub(x, pattern = "NA", replacement = NA)
  }))
  
  metaData <- filterUninformativeCols(metaData)
  
  return(metaData)
}

splitCombinedVars <- function(metaData, colsToDivide, delimiter) {
  if (is.null(colsToDivide) || delimiter == "") {
    return(metaData)
  }
  for (colName in colsToDivide) {
    numElements <- max(sapply(metaData[,colName], function(x) {
      length(str_extract_all(x, delimiter)[[1]]) + 1
    }), na.rm = TRUE)
    #targetCol <- metaData[,colName]
    if (numElements > 1) {
      colNames <- NULL
      for (i in 1:numElements) {
        colNames <- c(colNames, paste(colName, i, sep = "."))
      }
      metaData <- separate(metaData, col = colName, into = colNames, sep = delimiter)
    }
  }
  
  metaData <- as.data.frame(apply(metaData, 2, function(x) {
    gsub(x, pattern = "NA", replacement = NA)
  }))
  
  metaData <- filterUninformativeCols(metaData)
  
  return(metaData)
}

reformat_columns <- function(metaData, toSplit, colsToSplit, toDivide, colsToDivide, delimiter, delimiter2) {
  
  if (toSplit && (!is.null(colsToSplit)) && delimiter != "" && !is.null(metaData)) {
    #delimiterInfo <- NULL
    #if (allButSplit) {
    #  colsToSplit <- if (is.null(colsToSplit)) colnames(metaData) else colnames(metaData[-which(colnames(metaData) %in% colsToSplit)])
      #colsToSplit <- colsToSplit[-which(colsToSplit == "evalSame")]
    #}
    
    metaData <- extractColNames(metaData, delimiter, colsToSplit)
    
  }
  
  if (toDivide && (!is.null(colsToDivide)) && delimiter2 != "" && !is.null(metaData)) {
    #numElements <- NULL
    
    #if (allButDivide) {
    #  colsToDivide <- if (is.null(colsToDivide)) colnames(metaData) else colnames(metaData[-which(colnames(metaData) %in% colsToDivide)])
      #colsToDivide <- colsToDivide[-which(colsToDivide == "evalSame")]
    #}
    
    #for (col in colsToDivide) {
    #  numElements[[col]] <- length(str_split(metaData[1, col], delimiter2)[[1]])
    #}
    #browser()
    #start_time <- Sys.time()
    #sapply(colsToDivide, function(col) {
    #  max(sapply(metaData[,col], function(x) {
    #    length(str_extract_all(x, delimiter2)[[1]]) + 1
    #  }), na.rm = TRUE)
    #})
    #end_time <- Sys.time()
    #print(end_time - start_time)
    
    metaData <- splitCombinedVars(metaData, colsToDivide, delimiter2)
  }
  
  metaData <- as.data.frame(apply(metaData, 2, function(x) {
    gsub(x, pattern = "NA", replacement = NA)
  }))
  
  metaData <- filterUninformativeCols(metaData)
  
  return(metaData)
}

filterCols <- function(metaData, varsToKeep) {
  if (length(varsToKeep) == 0) {
    return(NULL)
  } else {
    metaData <- metaData[which(colnames(metaData) %in% varsToKeep)]
    return(metaData)
  }
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
    showModal(
      modalDialog(
        HTML(
          paste0('<font color="red"> Cannot name ', old_name, ' "', new_name, 
                 '" because this would create duplicate column names, which is not allowed. </font>')
          ), 
        title = "Error", 
        footer = modalButton("OK")
      )
    )
  } else if (old_name %in% colnames(metaData)) {
    #offendingChars <- findOffendingChars(unname(newNames))
    #if (any(offendingChars)) {
    #  createAlert(session, "alert", "offendingChars",
    #              content = paste("The following characters were removed from", 
    #                              newNames[colName], 
    #                              "because they might cause problems later:", 
    #                              paste(names(offendingChars[which(offendingChars == T)]), collapse = ", ")))
    #newNames[1] <- str_replace_all(unname(newNames), "[^\._\s0-9A-Za-z]", ".")
    #}
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
  
  incProgress()
  
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
      
      clinical_data[,col_to_sub] <- gsub(clinical_data[,col_to_sub], 
                                         pattern = subs$To_Replace[i], 
                                         replacement = subs$New_Val[i], 
                                         fixed = !use_reg_ex)
    }
    incProgress()
  }
  clinical_data <- clinical_data %>% mutate_all(~ replace(., . == "", NA))
  incProgress()
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
    incProgress()
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
    incProgress()
    #keep these lines down here so it won't fool the if statement above
    values <- to_exclude[which(to_exclude %in% metaData$filter_var)]
    metaData <- if (!identical(values, character(0))) filter(metaData, !filter_var %in% values) else metaData
    incProgress()
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

replace_blank_cells <- function(values) {
  for (pattern in c("^ $", "^$")) {
    values <- str_replace(values, pattern, NA_character_)
  }
  values
}

quickTranspose <- function(dataToTranspose) {
  
  incProgress()
  if (any(duplicated(dataToTranspose$ID, incomparables = NA))) {
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
  
  #if (replaceCol == "ID") {
  #  return(data)
  #}
  
  #dataWRowNames <- data
  
  replacementWRowNames <- replacement[, c("ID", replaceCol)]
  replacementWRowNames$ID <- as.character(replacementWRowNames$ID)
  
  incProgress()
  
  mergedData <- inner_join(data, replacementWRowNames, by = "ID") %>%
    select(-ID) %>%
    dplyr::rename(ID = replaceCol) %>%
    select(ID, everything())
  
  incProgress()
  
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
  
  incProgress()
    
  return(mergedData)
}

findExprLabelColumns <- function(ftData) {
  allDiff <- as.logical(lapply(ftData, function(x){
    length(unique(x)[which(!is.na(unique(x)))]) == nrow(ftData)
  }))
  return(colnames(ftData[allDiff]))
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

advance_columns_view <- function(data, start, forward_distance, previous_view = NULL) {
  if (class(start) == "character") {
    start <- which(colnames(data) == start)
  }
  
  end_point <- start + (forward_distance - 1)
  
  if (end_point > ncol(data)) {
    end_point <- ncol(data)
  }
  
  if (start >= end_point ) {
    return(previous_view)
  }
  
  next_cols <- data[,start:end_point]
  
  if (!"ID" %in% colnames(next_cols)) {
    return(cbind(ID = data$ID, next_cols))
  } else {
    return(next_cols)
  }
  
}

retract_columns_view <- function(data, last_column, backward_distance, previous_view = NULL) {
  
  if (class(last_column) == "character") {
    last_column <- which(colnames(data) == last_column)
  }
  
  start_point <- last_column - (backward_distance - 1)
  
  if (start_point < 1) {
    start_point <- 1
  }
  
  if (start_point >= last_column) {
    return(previous_view)
  }
  
  prev_cols <- data[,start_point:last_column]
  
  if (!"ID" %in% colnames(prev_cols)) {
    return(cbind(ID = data$ID, prev_cols))
  } else {
    return(prev_cols)
  }
}

#finds all of data1 in data2
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
