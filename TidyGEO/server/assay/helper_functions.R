#' Extract assay and feature data from the expressionSet object from GEO.
#'
#' @param expressionSet An object retrieved from GEO using getGEO.
#' @param session The shiny session that is calling this function.
#' @return A list with the assay dataset, the feature dataset, and whether there were any errors.
#' @examples
#' process_expression(my_expr_set, session)
process_expression <- function(expressionSet, session = NULL) {
  
  incProgress(message = "Extracting expression data")
  expression_raw <- assayData(expressionSet)$exprs
  incProgress(message = "Replacing blank values")
  if (nrow(expression_raw) == 1) {
    expressionData <- as.data.frame(t(as.matrix(apply(expression_raw, 2, replace_blank_cells))), stringsAsFactors = FALSE)
  } else {
    expressionData <- as.data.frame(apply(expression_raw, 2, replace_blank_cells), stringsAsFactors = FALSE)
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
      #browser()
      FALSE
    }
  })
  if (!pass && !is.null(session)) {
    # NOTE: It would seem that some alertIds are reserved ("parseError", for example), so if createAlert
    # isn't working, try changing the alertId.
    createAlert(session, anchorId = "alpha_alert", alertId = "nonnumeric", title = "Warning",
                content = paste("Some of the data is non-numeric.",
                                "This data is not supported by our some of our functionality.",
                                "Feel free to download the data to edit with another application."), append = FALSE)
  }
  
  expressionData <- cbind("ID" = rownames(expression_raw), expressionData)
  
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

#' Transpose a data frame; faster than <code>t()</code>.
#'
#' @param dataToTranspose A data frame with a column named 'ID'.
#' @return The data frame, transposed.
#' @examples
#' quickTranspose(data.frame(ID = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9)))
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

#' Replace the ID column in an assay dataset with the specified column from the feature data.
#'
#' @param data A data frame with the ID column to replace.
#' @param replacement The feature data with the column to replace the ID column, and its own ID column that matches the assay data's.
#' @param replaceCol The name of the column in the feature data that will replace the ID column.
#' @param summaryOption How to deal with duplicates in the new ID column: "mean", "median", "max", "min", or "keep all".
#' @param dropNA TRUE/FALSE, drop NA values from the replacement before replacing the ID column?
#' @return The assay data frame with a new ID column.
#' @examples
#' replaceID(data.frame(ID = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9)), 
#' data.frame(ID = c(1, 2, 3), B = c("a", "b", "c")), 
#' "B", 
#' "keep all", 
#' FALSE)
replaceID <- function(data, replacement, replaceCol, summaryOption, dropNA) {
  
  #if (replaceCol == "ID") {
  #  return(data)
  #}
  
  #dataWRowNames <- data
  if (dropNA) {
    #browser()
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

#' Drop rows from the data that don't meet specified criteria.
#'
#' @param data A data frame.
#' @param shinyFilterSpecs A list with the rows to keep. In the format of the list returned by input$DT_search_columns, with the columns to filter as names for the list.
#' @return The data, filtered.
#' @examples
#' filterExpressionData(data.frame(A = c("a", "b", "c"), B = c(4, 5, 6), C = c(7, 8, 9)), c(A = ["a", "b"], B = "4 ... 5"))
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

#' Create a preview of the next (forward_distance) columns of the data.
#'
#' @param data The full data frame to subset.
#' @param start The name or index of the first column in the subset.
#' @param forward_distance The number of columns after the start (including the start) to include in the subset.
#' @param previous_view The subset that was being displayed before this one.
#' @return A data frame that is the subset of the data that includes (start) to (forward_distance) columns.
#' @examples
#' advance_columns_view(data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9)), 0, 2)
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

#' Create a preview of the previous (backward_distance) columns in the data.
#'
#' @param data The full data frame to subset.
#' @param last_column The name or index of the last column in the subset.
#' @param backward_distance The number of columns, previous to last_column (and including last_column) to include in the subset.
#' @param previous_view The subset that was being displayed before this one.
#' @return A data frame that is the subset of the data that includes (backward_distance) to (last_column) columns.
#' @examples
#' help_modal("My_Help_File.md", "my_images_id")
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

#' Filter data1 to have only the rows in which the ID column matches data2's ID column.
#'
#' @param data1 The data frame to match to the other one.
#' @param data2 The data frame to use as a template to filter data1.
#' @param id_col1 The column name to use as the ID for data1. This will be matched with the ID column in data2.
#' @param id_col2 The column name to use as the ID for data2. This column will be used as filtering criteria for data1.
#' @return A data frame that has only the rows in which data1's ID entries exist in data2's ID column.
#' @examples
#' find_intersection(data.frame(ID = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9)), data.frame(ID = c(1, 3), B = c("a", "c")))
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

replace_NA_id <- function() {
  replace_na()
}