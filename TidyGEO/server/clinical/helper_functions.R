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

filterCols <- function(metaData, varsToKeep) {
  if (length(varsToKeep) == 0) {
    return(NULL)
  } else {
    metaData <- metaData[which(colnames(metaData) %in% varsToKeep)]
    return(metaData)
  }
}

#findOffendingChars <- function(x){
#  offendingChars <- c("?", "$", "/", "#", "=", "'", "%", "*", "^", "@", "!", "&", "(", ")")
#  myChars <- NULL
#  for (char in offendingChars) {
#    myChars[[char]] <- (grepl(paste0("\\", char), x))
#  }
#  return(myChars)
#}

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

#fixSpecialCharacters <- function(x, offendingChars)
#{
#  for (char in offendingChars) {
#    x <- str_replace(x, paste0("\\", char), "")
#  }
#  
#  return(x)
#}