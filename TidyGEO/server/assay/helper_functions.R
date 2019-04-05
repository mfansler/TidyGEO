process_expression <- function(expressionSet, session = NULL) {
  
  incProgress(message = "Extracting expression data")
  expression_raw <- assayData(expressionSet)$exprs
  incProgress(message = "Replacing blank values")
  if (nrow(expression_raw) == 1) {
    expressionData <- data.frame("ID" = rownames(expression_raw), 
                                 t(as.matrix(apply(expression_raw, 2, replace_blank_cells))), stringsAsFactors = FALSE)
  } else {
    expressionData <- data.frame("ID" = rownames(expression_raw),
                                 apply(expression_raw, 2, replace_blank_cells), stringsAsFactors = FALSE)
  }
  incProgress(message = "Formatting numeric data")
  pass <- tryCatch({
    if (nrow(expressionData) == 1) {
      expressionData <- as.data.frame(t(as.matrix(apply(expressionData, 2, as.numeric))))
    } else {
      expressionData <- as.data.frame(apply(expressionData, 2, as.numeric))
    }
    TRUE
  }, warning = function(w) {
    if (grepl("NAs introduced by coercion", w)) {
      FALSE
    }
  })
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
  }
  
  if (nrow(featureData) == 1) {
    featureData <- as.data.frame(t(as.matrix(apply(featureData, 2, replace_blank_cells))), stringsAsFactors = FALSE)
  } else {
    featureData <- as.data.frame(apply(featureData, 2, replace_blank_cells), stringsAsFactors = FALSE)
  }
  
  rows_to_keep <- sapply(1:nrow(featureData), function(i) {
    if (!all(is.na(featureData[i,]))) {
      i
    }
  })
  featureData <- featureData[unlist(rows_to_keep),]
  
  if (nrow(expressionData) == 0) {
    expressionData <- NULL
  }
  if (nrow(featureData) == 0) {
    featureData <- NULL
  }
  
  return(list("expressionData" = expressionData, "featureData" = featureData, "status" = pass))
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

replaceID <- function(data, replacement, replaceCol, summaryOption, dropNA) {
  
  #if (replaceCol == "ID") {
  #  return(data)
  #}
  
  #dataWRowNames <- data
  if (dropNA) {
    browser()
    replacementWRowNames <- replacement[!is.na(replacement[,replaceCol]), c("ID", replaceCol)]
  } else {
    replacementWRowNames <- replacement[, c("ID", replaceCol)]
  }
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

#findExprLabelColumns <- function(ftData) {
#  allDiff <- as.logical(lapply(ftData, function(x){
#    length(unique(x)[which(!is.na(unique(x)))]) == nrow(ftData)
#  }))
#  return(colnames(ftData[allDiff]))
#}

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