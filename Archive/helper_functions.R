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
                    "url" = function(x) all(!grepl("(((https?)|(ftp)):\\/\\/)|www\\.", x)),
                    "dates" = function(x) all(!grepl("[A-Za-z]+ [0-9]{1,2},? [0-9]{2,4}", x)),
                    "same_vals" = function(x) length(unique(as.factor(as.character(toupper(x))))) > 1,
                    "all_diff" = function(x) if (isAllNum(x)) TRUE else length(unique(as.factor(as.character(toupper(x))))) != total_rows,
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

extractColNames <- function(input_df, delimiter, colsToSplit, use_regex = FALSE) {
  if (is.null(colsToSplit) || delimiter == "") {
    return(input_df) 
  }
  
  errorMessage <- NULL
  
  # Save the rownames as a column so the tidyverse functions don't get rid of them
  input_df <- cbind(row_names = rownames(input_df), input_df)
  metaData <- input_df
  num_split <- 0
  
  # Whether to treat the delimiter as literal or not
  regex_delimiter <- if (!use_regex) paste0("\\Q", delimiter, "\\E") else delimiter
  
  tryCatch({
    for (col in colsToSplit) {
      # Check if every row has the delimiter (if not, separate won't work)
      hasDelim <- is.na(metaData[, col]) | str_detect(metaData[, col], regex_delimiter)
      
      if (all(hasDelim)) {
        
        # the key column now has the new column names and the value column has the values
        metaData <- separate(metaData, col, sep = regex_delimiter, into = c("key", "value"), extra = "merge") %>%
          mutate(key = str_trim(key), value = str_trim(value))
        metaData$value <- str_replace_all(metaData$value, "NA", NA_character_)
        
        # if any of the keys are column names we've seen before
        for (duplicate in unique(metaData$key)[which(unique(metaData$key) %in% colnames(metaData))]) {
          # try to merge the duplicate with the column that already exists
          new_data <- cbind(metaData, vals_to_merge = if_else(metaData$key == duplicate, metaData$value, NA_character_))
          results <- shift_cells(new_data, "vals_to_merge", duplicate)
          if (is.null(results[["conflicts"]])) { # make sure the duplicate won't overwrite any values in the original column
            metaData <- results[["result"]]
            metaData$key <- str_replace_all(metaData$key, duplicate, NA_character_)
            num_split <- num_split + 1
          } else {# if the duplicate will overwrite values in the original column, make a new column with a unique name
            new_names <- make.unique(c(colnames(metaData), duplicate))
            metaData$key <- str_replace(metaData$key, duplicate, new_names[length(new_names)])
          }
        }
        if (all(c("key", "value") %in% colnames(metaData))) { # i.e. these columns weren't removed in the above code
          
          # this section makes sure the columns end up in the same order that they appeared originally
          col_names <- colnames(metaData)
          col_names <- append(col_names, unique(metaData$key)[which(!is.na(unique(metaData$key)))], which(col_names == "key"))
          col_names <- col_names[-which(col_names %in% c("key", "value"))]
          
          # creates new columns, with the key column as the new column names and the value column as the values
          metaData <- spread(metaData, key = "key", value = "value")
          metaData <- metaData[,col_names]
          
          num_split <- num_split + 1
          
        } else if (any(!is.na(metaData$key))) { # we have keys that don't have any corresponding values
          stop("You did something wrong.")
        }
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
    errorMessage <<- 'Something went wrong. Try running again with the "Use regex" option <b>unchecked</b>.'
    metaData <<- input_df
    num_split <<- 0
  })
  
  
  if (!is.null(errorMessage)) {
    if (!any(str_detect(errorMessage, 'Something went wrong'))) {
      errorMessage <- c(paste0('<b>Looks like there are some cells that don\'t contain the delimiter "', delimiter, '".</b>'),
                        errorMessage)
    }
    errorMessage <- paste(errorMessage, collapse = "<br/>")
    errorMessage <- paste('<font color="red">', errorMessage, '</font>')
    showModal(
      modalDialog(HTML(errorMessage), title = HTML('<font color="red">Whoops!</font>'), footer = modalButton("OK")
      )
    )
  }
  
  # put the row names back where they belong
  metaData <- data.frame(metaData[,-1], row.names = metaData$row_names, check.names = FALSE)
  
  if (num_split > 0) { # i.e. we actually changed the data
    
    colnames(metaData) <- col_names[-1]
    
    #metaData <- as.data.frame(apply(metaData, 2, function(x) {
    #  gsub(x, pattern = "NA", replacement = NA)
    #}))
    
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
        if (!is.na(x) && len > nchar(x)) {
          stop("You have made a mistake.")
        } else {
          len
        }
      }), na.rm = TRUE)
      #targetCol <- metaData[,colName]
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
    errorMessage <<- 'Something went wrong. Try running again with the "Use regex" option <b>unchecked</b>.'
    metaData <<- input_df
    num_split <<- 0
  })
  
  
  if (!is.null(errorMessage)) {
    if (!str_detect(errorMessage, 'Something went wrong')) {
      errorMessage <- c(paste0('<b>Looks like there are some cells that don\'t contain the delimiter "', delimiter, '".</b>'),
                        errorMessage)
    }
    errorMessage <- paste(errorMessage, collapse = "<br/>")
    errorMessage <- paste('<font color="red">', errorMessage, '</font>')
    showModal(
      modalDialog(HTML(errorMessage), title = HTML('<font color="red">Whoops!</font>'), footer = modalButton("OK")
      )
    )
  }
  
  if (num_split > 0) {
    #metaData <- as.data.frame(apply(metaData, 2, function(x) {
    #  gsub(x, pattern = "NA", replacement = NA)
    #}))
    
    metaData <- filterUninformativeCols(metaData)
  }
  
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
  
  if (old_name %in% colnames(metaData)) {
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
  
  subs$New_Val <- str_replace_na(subs$New_Val, replacement = "")
  
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
            filter_var <- as.numeric(as.character(filter_var))
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

shift_cells <- function(data, col1, col2, conflicts = NULL) {
  results <- list()
  if (is.null(conflicts)) {
    conflict_indices <- !is.na(data[,col1]) & !is.na(data[,col2]) 
    if (any(conflict_indices)) {
      results[["conflicts"]] <- data[conflict_indices, c(col1, col2)]
    }
    # The !! operator unquotes its argument. It gets evaluated immediately in the surrounding context.
    data <- unite(data, col2, col1, col = !!col2)
    data[,col2] <- str_remove_all(data[,col2], "_NA|NA_")
    data[,col2] <- str_replace_all(data[,col2], "NA", NA_character_)
  } else if (conflicts == col1) {
    shift_indices <- which(!is.na(data[,col1]))
    data[shift_indices, col2] <- data[shift_indices, col1]
    data[shift_indices, col1] <- rep(NA, length(shift_indices))
  } else if (conflicts == col2) {
    shift_indices <- which(is.na(data[,col2]))
    data[shift_indices, col2] <- data[shift_indices, col1]
    data[shift_indices, col1] <- rep(NA, length(shift_indices))
  } else if (conflicts == "keepall") {
    id_exists <- "ID" %in% colnames(data)
    if (!id_exists) {
      data <- cbind("ID" = rownames(data), data)
    }
    data2 <- select(data, ID, !!col1)
    data <- merge(select(data, -!!col1), data2, by.x = c("ID", col2), by.y = c("ID", col1), all = TRUE)
    if (!id_exists) {
      rownames(data) <- data[,"ID"]
      data <- select(data, -ID)
    }
  } else {# conflicts = delimiter
    data <- unite(data, col1, col2, col = !!col2, sep = conflicts)
    data[,col2] <- str_remove_all(data[,col2], paste0(conflicts, "NA|NA", conflicts))
  }
  results[["result"]] <- data
  return(results)
}

shifting_cells = function(all_data, pattern, col_names, new_colum_name) {
  data_needed = all_data[col_names]
  x = function(a) {
    b = grep(pattern, a, value = TRUE)
    #print(pattern)
    #print(b)
    if (length(b) == 0) {
      return (NA)
    } else if (length(b) > 1){
      return (b[1])
    } else {
    return (b)}
  }
 
  last_col = length(col_names)
  new_colum = apply(data_needed, 1, x)
  all_data = add_column(all_data, new_colum, .after = col_names[last_col])
 # all_data$new_colum_name = apply(data_needed, 1, x)
  colnames(all_data)[colnames(all_data) == "new_colum"] = new_colum_name
  return (all_data)
}

