in_app <- TRUE

# Retrieve the platform information for a dataset -------------------------
# Library dependencies: GEOquery; stringr
# Function dependencies:
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

# A helper function for getGEO --------------------------------------------
# Library dependencies: GEOquery
# Function dependencies:
getAndParseGSEMatrices <- function(GEO, destdir, AnnotGPL, getGPL = TRUE, 
                                   parseCharacteristics = TRUE, platform = NULL) 
{ # Please note: this function was copied from the GEOquery package. It was altered
  # to only download one platform (rather than all the platforms) from GEO and to
  # delete the temp file after downloading.
  GEO <- toupper(GEO)
  stub = gsub("\\d{1,3}$", "nnn", GEO, perl = TRUE)
  if (is.null(platform)) {
    gdsurl <- "https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/"
    b = GEOquery:::getDirListing(sprintf(gdsurl, stub, GEO))
    platform <- b[1]
  }
  if (in_app) incProgress(message = "Loading GSE file")
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
  if (in_app) incProgress(message = "Parsing GSE matrix")
  result <- GEOquery:::parseGSEMatrix(destfile, destdir = destdir, 
                                      AnnotGPL = AnnotGPL, getGPL = getGPL)$eset
  file.remove(destfile)
  return(result)
}

# Get expressionSet object from GEO ---------------------------------------
# Library dependencies: GEOquery
# Function dependencies: getAndParseGSEMatrices
getGEO <- function(GEO = NULL, filename = NULL, destdir = tempdir(), 
                   GSElimits = NULL, GSEMatrix = TRUE, AnnotGPL = FALSE, getGPL = TRUE, 
                   parseCharacteristics = TRUE, platform = NULL) 
{ # Please note: this function was copied from the GEOquery package. It was altered
  # to only download one platform (rather than all the platforms) from GEO and to
  # delete the temp file after downloading.
  con <- NULL
  if (!is.null(GSElimits)) {
    if (length(GSElimits) != 2) {
      stop("GSElimits should be an integer vector of length 2, like (1,10) to include GSMs 1 through 10")
    }
  }
  if (is.null(GEO) & is.null(filename)) {
    stop("You must supply either a filename of a GEO file or a GEO accession")
  }
  if (in_app) incProgress(message = "Fetching GSE file")
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

# Get expressionSet object for a given GSE and platform -------------------
# Library dependencies: GEOquery
# Function dependencies: getGEO
load_series <- function(geoID, platform, session = NULL) {
  
  #expressionSet <- loadRdsFromDropbox(geoID)
  #expressionSet <- NULL
  
  #if (is.null(expressionSet)) {
    eSet_status <- tryCatch({
      if (!grepl("GSE", geoID, ignore.case = TRUE)) {
        stop('Please enter an ID that begins with "GSE".', call. = FALSE)
      }
      #expressionSet <- getGEO(GEO = geoID, GSEMatrix = TRUE, getGPL = TRUE, AnnotGPL = TRUE)
      #browser()
      if (in_app) incProgress(message = "Loading series files")
      expressionSet <- getGEO(geoID, platform = platform)
      #saveDataRDS(expressionSet, paste0(geoID, ".rds"))
      list(eSet = expressionSet, status = "pass")
    }, error = function(e){
      if (grepl("open\\.connection", paste0(e))) {
        status <- "Trouble establishing connection to GEO. Please try again later."
        return(list(eSet = NULL, status = status))
      }
      else if (grepl("file\\.exists", paste0(e))) {
        status <- "File not found. Please enter a valid ID."
        return(list(eSet = NULL, status = status))
      }
      else {
        status <- paste(e, collapse = " ")
        return(list(eSet = NULL, status = status))
      }
    }
    )
    
    if (in_app) incProgress(message = "Evaluating success")
    if (!is.null(session)) {
      if (eSet_status$status != "pass") {
        title <- "Error"
        content <- eSet_status$status
      } else {
        title <- "Success!"
        content <- "Series data successfully downloaded. You may now work with the Clinical and/or Assay data using the buttons and menu options to the left."
      }
      createAlert(session, "alert", "fileError", title = title,
                  content = content, append = FALSE)
    } else if (eSet_status$status != "pass") {
        stop(eSet_status$status)
    }
  #}
  return(eSet_status$eSet)
}

# Check if a column is all numeric ----------------------------------------
# Library dependencies:
# Function dependencies:
isAllNum <- function(metaData) {
  #toEvaluate <- metaData[which(!is.na(metaData)),]
  if (typeof(metaData) == "list") {
    metaData <- unlist(metaData)
  }
  toEvaluate <- na.omit(metaData)
  vals <- unique(toEvaluate)
  temp <- suppressWarnings(as.numeric(as.character(toEvaluate)))
  isNum <- all(is.numeric(temp)) && all(!is.na(temp)) && length(vals) > 2
  return(isNum)
}

# Check if all values in a column are unique ------------------------------
# Library dependencies:
# Function dependencies:
is_all_unique <- function(my_list) {
  if (typeof(my_list) == "list") {
    my_list <- unlist(my_list)
  }
  vals <- unique(my_list[which(!is.na(my_list))])
  return(length(vals) == length(my_list))
}

# Check if all values in a column are identical ---------------------------
# Library dependencies:
# Function dependencies:
is_all_identical <- function(my_list) {
  if (typeof(my_list) == "list") {
    my_list <- unlist(my_list)
  }
  vals <- unique(my_list[which(!is.na(my_list))])
  return(length(vals) == 1)
}

# Replace empty strings with NA -------------------------------------------
# Library dependencies: stringr
# Function dependencies:
replace_blank_cells <- function(values) {
  if (any(str_detect(values, "^ *$"), na.rm = TRUE)) {
    str_replace_all(values, "^ *$", NA_character_)
  } else {
    values
  }
}

# Extract clinical data from expressionSet object -------------------------
# Library dependencies: GEOquery
# Function dependencies: replace_blank_cells; filterUninformativeCols
process_clinical <- function(expressionSet, session = NULL) {
  if (in_app) incProgress(message = "Extracting data")
  metaData <- pData(expressionSet)
  if (nrow(metaData) == 1) {
    metaData <- as.data.frame(t(as.matrix(apply(metaData, 2, replace_blank_cells))), row.names = rownames(metaData), stringsAsFactors = FALSE)
  } else {
    metaData <- as.data.frame(apply(metaData, 2, replace_blank_cells), row.names = rownames(metaData), stringsAsFactors = FALSE)
  }
  if (in_app) incProgress(message = "Filtering data")
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

# Mark which columns in the dataset are informative -----------------------
# Library dependencies:
# Function dependencies: isAllNum
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

# Drop columns from the data that are not informative ---------------------
# Library dependencies:
# Function dependencies: evaluate_cols_to_keep
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

# Split key-value pairs ---------------------------------------------------
# Library dependencies: stringr; tidyr; dplyr
# Function dependencies: shift_cells; filterUninformativeCols
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
    if (in_app) {
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
    } else {
      print(errorMessage)
    }
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

# Split columns that have multiple values separated by a delimiter --------
# Library dependencies: stringr; tidyr
# Function dependencies: filterUninformativeCols
splitCombinedVars <- function(input_df, colsToDivide, delimiter, use_regex = FALSE) {
  # Step 1: prevent splitting if input is empty
  if (is.null(colsToDivide) || delimiter == "") {
    return(input_df)
  }
  # Step 2: prep variables needed
  metaData <- input_df
  num_split <- 0
  errorMessage <- NULL
  regex_delimiter <- if (!use_regex) paste0("\\Q", delimiter, "\\E") else delimiter
  tryCatch({
    #browser()
    for (colName in colsToDivide) {
      # Step 3: separate out the rows that actually have the delimiter
      contains_delim <- str_detect(pull(metaData, colName), regex_delimiter)
      dont_split <- metaData[,colName][!contains_delim]
      metaData[,colName][!contains_delim] <- NA
      
      # Step 4: generate the column names for the new columns
      numElements <- max(sapply(metaData[,colName], function(x) {
        len <- length(str_extract_all(x, regex_delimiter)[[1]]) + 1
        if (!is.na(x) && len > nchar(x)) {
          stop("Error in splitCombinedVars.")
        } else {
          len
        }
      }), na.rm = TRUE)
      #if (numElements > 1) {
      colNames <- make.unique(c(colnames(metaData), rep(colName, numElements)))
      colNames <- colNames[(length(colnames(metaData)) + 1):length(colNames)]
      #if (any(colNames %in% colnames(metaData))) {
      #  colNames[which(colNames %in% colnames(metaData))] <- paste(colNames[which(colNames %in% colnames(metaData))], 2, sep = ".")
      #}
      #for (i in 1:numElements) {
      #  colNames <- c(colNames, paste(colName, i, sep = "."))
      #}
      
      # Step 5: split the column that has all the delimiters
      metaData <- separate(metaData, col = colName, into = colNames, sep = regex_delimiter, fill = "right")
      if (length(dont_split) > 0 && paste0(colName, ".1") %in% colnames(metaData)) {
        metaData[,paste0(colName, ".1")][!contains_delim] <- dont_split
      }
      
      # Step 6: put the rows without the delimiters back where they belong
      num_split <- num_split + 1
      #} else {
      #  offendingVals <- paste(metaData[, colName], collapse = ", ")
      #  offendingVals <- if_else(nchar(offendingVals) > 50, paste0(substr(offendingVals, 1, 50), "... "), offendingVals)
      #  
      #  errorMessage <- c(errorMessage, paste0(colName, " could not be split."),
      #                    paste0("Values: ", offendingVals))
      #}
    }
  }, error = function(e) {
    errorMessage <<- paste('Something went wrong.', paste(e, collapse = " "), sep = "\n")
    metaData <<- input_df
    num_split <<- 0
  })
  
  
  if (!is.null(errorMessage)) {
    if (in_app) {
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
    } else {
      print(errorMessage)
    }
  }
  
  if (num_split > 0) {
    
    metaData <- filterUninformativeCols(metaData)
  }
  
  return(metaData)
}

# Keep only the specified columns -----------------------------------------
# Library dependencies:
# Function dependencies:
filterCols <- function(metaData, varsToKeep) {
  if (length(varsToKeep) == 0) {
    return(NULL)
  } else {
    metaData <- metaData[which(colnames(metaData) %in% varsToKeep)]
    return(metaData)
  }
}


# Change a column name ----------------------------------------------------
# Library dependencies:
# Function dependencies:
renameCols <- function(metaData, old_name, new_name) {
  if (old_name %in% colnames(metaData)) {
    new_name <- str_replace_all(new_name, "[\\\\\\/:\\*\\?\\<\\>\\=\\+\\#\\~\\`\\'\\;\\&\\%\\$\\@\\!]", "_")
    colnames(metaData)[which(colnames(metaData) == old_name)] <- new_name
  }
  return(metaData)
}

# Replace values with different values, for one column --------------------
# Library dependencies: stringr; dplyr
# Function dependencies:
substitute_vals <- function(clinical_data, sub_specs, use_reg_ex = FALSE)
{
  col_to_sub <- names(sub_specs) 
  subs <- sub_specs[[col_to_sub]]
  row_names <- rownames(clinical_data)
  clinical_data[,col_to_sub] <- as.character(clinical_data[,col_to_sub])
  
  if (in_app) incProgress(message = "Reformatting missing values")
  
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
    if (in_app) incProgress()
  }
  clinical_data <- clinical_data %>% mutate_all(~ replace(., . == "", NA))
  if (in_app) incProgress()
  rownames(clinical_data) <- row_names
  return(clinical_data)
}

# Filter out (or keep only) rows that match the specifications ------------
# Library dependencies: dplyr; stringr
# Function dependencies:
excludeVars <- function(metaData, variable, to_exclude) {
  metaData <- cbind(ID = rownames(metaData), metaData)
  #tryCatch({ #for debugging purposes, take out in final product
    metaData <- dplyr::rename(metaData, filter_var = variable)
    if (any(to_exclude == "NA")) {
      metaData <- filter(metaData, !is.na(filter_var))
    }
    if (in_app) incProgress(message = "Determining columns to exclude")
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
    if (in_app) incProgress(message = "Dropping columns")
    #keep these lines down here so it won't fool the if statement above
    values <- to_exclude[which(to_exclude %in% metaData$filter_var)]
    metaData <- if (!identical(values, character(0))) filter(metaData, !filter_var %in% values) else metaData
    if (in_app) incProgress(message = "Structuring data")
    colnames(metaData)[which(colnames(metaData) == "filter_var")] <- variable
  #}, error = function(e) {
  #  print(e)
  #})
  rownames(metaData) <- metaData$ID
  metaData <- metaData[-which(colnames(metaData) == "ID")]
  return(metaData)
}


# Move values into the correct column -------------------------------------
# Library dependencies: tidyr; stringr; dplyr
# Function dependencies
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

# Extract assay data from expressionSet object --------------------------
# Library dependencies: GEOquery
# Function dependencies: replace_blank_cells
process_expression <- function(expressionSet, session = NULL) {
  
  if (in_app) incProgress(message = "Extracting expression data")
  expression_raw <- assayData(expressionSet)$exprs
  
  if (in_app) incProgress(message = "Parsing expression data")
  expressionData <- if (nrow(expression_raw) == 0) {
    pass <- TRUE
    NULL
  } else {
    if (in_app) incProgress(message = "Parsing expressionSet object from GEO")
    
    intermed_data <- if (typeof(expression_raw) != "double") { # only attempt to convert if character
      if (in_app) incProgress(message = "Attempting to convert data to numeric")
      result <- tryCatch({
        if (nrow(expressionData) == 1) {
          # apply does weird things to datasets with only 1 row
          t(as.matrix(apply(expression_raw, 2, parse_double)))
        } else {
          # parse_double simultaneously gets rid of extra spaces and converts to numeric
          # this is faster than writing to a temp file and reading back with read_csv
          # and faster than getting rid of spaces and using as.numeric
          apply(expression_raw, 2, parse_double)
        }
      }, warning = function(w) { # some of the data is non-numeric
        if (grepl("parsing failures", w)) {
          NULL
        }
      })
      if (all(is.null(result))) { # the data was not converted to numeric
        pass <- FALSE
        expression_raw # keep the original data
      } else {
        pass <- TRUE
        result # use the converted data
      }
    } else {
      pass <- TRUE
      expression_raw
    }
    
    if (in_app) incProgress(message = "Structuring data")
    cbind.data.frame("ID" = rownames(expression_raw), intermed_data, stringsAsFactors = FALSE)
  }
  
  return(list("expressionData" = expressionData, "status" = pass))
}

# Extract feature data from expressionSet object --------------------------
# Library dependencies: stringr; readr; janitor
# Function dependencies:
process_feature <- function(expressionSet, session = NULL) {
  if (in_app) incProgress(message = "Extracting feature data")
  featureData <- data.frame(fData(expressionSet))
  if (!"ID" %in% colnames(featureData)) {
    featureData <- cbind.data.frame(ID = rownames(featureData), featureData, stringsAsFactors = FALSE)
  }
  if (in_app) incProgress(message = "Replacing blank cells")
  if (nrow(featureData) == 1) {
    # apply does weird things to datasets with only 1 row
    t(as.matrix(apply(featureData, 2, parse_character)))
  } else {
    # parse_double simultaneously gets rid of extra spaces and converts to numeric
    # this is faster than writing to a temp file and reading back with read_csv
    # and faster than getting rid of spaces and using as.numeric
    apply(featureData, 2, parse_character)
  }
  
  if (in_app) incProgress(message = "Dropping empty columns")
  # remove_empty is a function from the "janitor" package that drops
  # columns that are all `NA`
  featureData <- remove_empty(featureData, which = "cols")
  
  
  if (nrow(featureData) == 0) {
    featureData <- NULL
  }
  return(featureData)
}

# A faster way to transpose than t() --------------------------------------
# Library dependencies: tidyr; dplyr
# Function dependencies:
quickTranspose <- function(dataToTranspose, force = FALSE) {
  # We'll take dataToTranspose and make the "ID" column the column
  # names, and the column names the "ID" column.
  
  
  if (in_app) incProgress(message = "Transposing data")
  # Make sure the ID column is there, and add it if not.
  if (!"ID" %in% colnames(dataToTranspose)) {
    dataToTranspose$ID <- rownames(dataToTranspose)
  }
  # If any of the ID values are duplicates, we can't use them as column names
  # (column names must be unique).
  if (any(duplicated(dataToTranspose$ID, incomparables = NA))) {
    if (force) { # Make the ID values unique.
      message <- "The ID column is not all unique. Appending numbers to duplicates to make unique for transpose."
      if (in_app) showNotification(message) else print(message)
      dataToTranspose$ID <- make.unique(dataToTranspose$ID)
    } else { # Inform the user that we didn't perform the operation.
      message <- "Data cannot be transposed because the ID column is not all unique."
      if (in_app) showNotification(message) else print(message)
      return(dataToTranspose)
    }
  }
  # Transpose the data.
  transposed <- dataToTranspose %>%
    gather(newrows, valname, -ID) %>%
    spread(ID, valname) %>%
    dplyr::rename(ID = "newrows")
  
  
  if (in_app) incProgress(message = "Transposing data")
  return(transposed)
}

# Replace the assay ID's with a different column from feature data --------
# Library dependencies: dplyr
# Function dependencies:
replaceID <- function(data, replacement, replaceCol, summaryOption, dropNA) {
  
  if (in_app) incProgress(message = "Formatting replacement")
  if (!is.null(dropNA) && dropNA) {
    replacementWRowNames <- replacement[!is.na(replacement[,replaceCol]), c("ID", replaceCol)]
  } else {
    replacementWRowNames <- replacement[, c("ID", replaceCol)]
  }
  replacementWRowNames$ID <- as.character(replacementWRowNames$ID)
  
  if (in_app) incProgress(message = "Merging changes")
  mergedData <- inner_join(data, replacementWRowNames, by = "ID") %>%
    select(-ID) %>%
    dplyr::rename(ID = replaceCol) %>%
    select(ID, everything())
  
  if (in_app) incProgress(message = "Summarizing data")
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
  
  if (in_app) incProgress(message = "Returning result")
  return(mergedData)
}

# Save the filters from a DT object ---------------------------------------
# Library dependencies: stringr
# Function dependencies:
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

# Move the viewer forward to the next columns -----------------------------
# Library dependencies:
# Function dependencies:
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

# Move the viewer backward to the previous columns ------------------------
# Library dependencies:
# Function dependencies:
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

# Line up two datasets according to an ID column --------------------------
# Library dependencies:
# Function dependencies:
find_intersection <- function(data1, data2, id_col1 = "ID", id_col2 = NULL) {
  search_terms <- if (is.null(id_col2))
                    data2
                  else if (grepl("col.*names", id_col2))
                    colnames(data2)
                  else if (grepl("row.*names", id_col2))
                    rownames(data2)
                  else
                    data2[,id_col2]
  
  data1_edited <- if (grepl("col.*names", id_col1)) {
    data1[,which(colnames(data1) %in% c(search_terms, "ID"))]
  } else if (grepl("row.*names", id_col1)) {
    data1[which(rownames(data1) %in% search_terms),]
  } 
  else {
    data1[which(data1[,id_col1] %in% search_terms),]
  }
  if (is.null(length(data1_edited) > 0) || is.null(nrow(data1_edited) > 0)) {
    browser()
  }
  if (length(data1_edited) > 0 && nrow(data1_edited) > 0) {
    if (in_app) {
      showModal(
        modalDialog(
          title = HTML(paste0('<font color="green">', 'Success!', '</font>')),
          p("Entries in each dataset were successfully filtered to match each other.", style="color:green"),
          easyClose = TRUE
        )
      )
    }
    data1_edited
  } else {
    stop("No entries in the second dataset matched the entries in the first.")
    data1
  }
}

# Join up to three datasets -----------------------------------------------
# Library dependencies:
# Function dependencies:
join_data <- function(df1, df2, join_col1, join_col2, join_behavior) {
  if (in_app) incProgress(message = "Preparing data to join")
  if (join_col1 == "colnames") {
    df1 <- quickTranspose(df1, force = TRUE)
    join_col1 <- "ID"
  } else if (join_col1 == "rownames") {
    df1$rownames <- rownames(df1)
  }
  if (in_app) incProgress()
  if (join_col2 == "colnames") {
    df2 <- quickTranspose(df2, force = TRUE)
    join_col2 <- "ID"
  } else if (join_col2 == "rownames") {
    df2$rownames <- rownames(df2)
  }
  join_vars = c(join_col2)
  names(join_vars) <- join_col1
  
  if (in_app) incProgress(message = "Joining data")
  
  join_func <- if (join_behavior == "drop") {
    "inner_join"
  } else if (join_behavior == "keep values from first dataset") {
    "left_join"
  } else if (join_behavior == "keep values from second dataset") {
    "right_join"
  } else {
    "full_join"
  }
  result <- do.call(join_func, list(x = df1, y = df2, by = join_vars))
  
  if (in_app) incProgress(message = "Formatting joined data")
  
  if ("rownames" %in% colnames(result)) {
    rownames(result) <- result$rownames
    result <- result[-"rownames"]
  }
  return(result)
}
