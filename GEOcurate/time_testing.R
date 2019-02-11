setwd("C:/Users/Avery/Documents/R_Code/geocurate_repo/GEOcurate")

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
      metaData <- metaData[,cols_to_keep]
    }
  }
  metaData
}

evaluate_cols_to_keep2 <- function(col, toFilter = list()) {
  functions <- list("reanalyzed" = function(x) all(!str_detect(x, "Reanaly[sz]ed ")),
                    "url" = function(x) all(!str_detect(x, "ftp:\\/\\/")),
                    "dates" = function(x) all(!str_detect(x, "[A-Za-z]+ [0-9]{1,2},? [0-9]{2,4}")),
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

filterUninformativeCols2 <- function(metaData, toFilter = list())
{
  metaData <- metaData[!duplicated(as.list(metaData))]
  
  if (ncol(metaData) > 1) {
    
    cols_to_keep <- apply(metaData, 2, evaluate_cols_to_keep2, toFilter = toFilter)
    if (all(!cols_to_keep)) {
      print("No informative columns found.")
      NULL
    } else {
      metaData <- metaData[,cols_to_keep]
    }
  }
  metaData
}

# test data ---------------------------------------------------------------

setwd("~/R_Code/geocurate_repo/GEOcurate")
geoID <- "GSE3"
index = 1
expressionSet <- loadRdsFromDropbox(geoID)
expressionSet <- expressionSet[[index]]
metaData <- pData(expressionSet)
metaData <- as.data.frame(apply(metaData, 2, replace_blank_cells), row.names = rownames(metaData), stringsAsFactors = FALSE)
expressionData <- assayData(expressionSet)$exprs
expressionData <- data.frame("ID" = rownames(expressionData), apply(expressionData, 2, as.numeric))

# different kinds of apply (2 does not work) ------------------------------


toFilter <- c("reanalyzed", "url", "dates", "same_vals", "all_diff")
start_time <- Sys.time()
a <- filterUninformativeCols(metaData, toFilter)
end_time <- Sys.time()
print(paste("Original:", end_time - start_time))
start_time <- Sys.time()
b <- filterUninformativeCols2(metaData, toFilter)
end_time <- Sys.time()
print(paste("New:", end_time - start_time))

# apply vs str_detect -----------------------------------------------------


col <- "description.12"
delimiter <- "="
start_time <- Sys.time()
hasDelim <- as.logical(sapply(metaData[, col], function(x){
  is.na(x) || str_detect(x, delimiter)
}))
end_time <- Sys.time()
print(paste("Original:", end_time - start_time))
start_time <- Sys.time()
hasDelim2 <- is.na(metaData[, col]) | str_detect(metaData[, col], delimiter)
end_time <- Sys.time()
print(paste("New:", end_time - start_time))

# gsub vs dplyr -----------------------------------------------------------


col_to_sub <- "description.13"
subs <- data.frame(`To_Replace` = "kidney", `New_Val` = "kid")
use_reg_ex <- FALSE
i <- 1
clinical_data <- metaData

start_time <- Sys.time()
clinical_data[,col_to_sub] <- sapply(metaData[,col_to_sub], 
                                     function(x){
                                       gsub(x, pattern = subs$To_Replace[i], replacement = subs$New_Val[i], fixed = !use_reg_ex)
                                     })
end_time <- Sys.time()
print(paste("Original:", end_time - start_time))
start_time <- Sys.time()
clinical_data[,col_to_sub] <- gsub(metaData[,col_to_sub], pattern = subs$To_Replace[i], replacement = subs$New_Val[i], fixed = !use_reg_ex)
end_time <- Sys.time()
print(paste("Originalp2:", end_time - start_time))
#start_time <- Sys.time()
#clinical_data <- metaData %>% mutate_at(lazyeval::uq(col_to_sub), gsub(subs$To_Replace[i], subs$New_Val[i], ., fixed = !use_reg_ex))
#end_time <- Sys.time()
#print(paste("New:", end_time - start_time))

# list initialization -----------------------------------------------------


start_time <- Sys.time()
functions <- list("reanalyzed" = function(x) all(!grepl("Reanaly[sz]ed ", x)),
                  "url" = function(x) all(!grepl("ftp:\\/\\/", x)),
                  "dates" = function(x) all(!grepl("[A-Za-z]+ [0-9]{1,2},? [0-9]{2,4}", x)),
                  "same_vals" = function(x) length(unique(as.factor(as.character(toupper(x))))) > 1,
                  "all_diff" = function(x) length(unique(as.factor(as.character(toupper(x))))) != total_rows,
                  "tooLong" = function(x) {
                    isTooLong <- as.logical(nchar(x) > 100)
                    sum(isTooLong) < (length(x) / 2)
                  })
end_time <- Sys.time()
print(paste("Original:", end_time - start_time))
start_time <- Sys.time()
functions <- c("reanalyzed" = function(x) all(!grepl("Reanaly[sz]ed ", x)),
                  "url" = function(x) all(!grepl("ftp:\\/\\/", x)),
                  "dates" = function(x) all(!grepl("[A-Za-z]+ [0-9]{1,2},? [0-9]{2,4}", x)),
                  "same_vals" = function(x) length(unique(as.factor(as.character(toupper(x))))) > 1,
                  "all_diff" = function(x) length(unique(as.factor(as.character(toupper(x))))) != total_rows,
                  "tooLong" = function(x) {
                    isTooLong <- as.logical(nchar(x) > 100)
                    sum(isTooLong) < (length(x) / 2)
                  })
end_time <- Sys.time()
print(paste("New:", end_time - start_time))

# which vs direct indexing ------------------------------------------------


metaData_test <- cbind(metaData, testcol = sample(1:nrow(metaData), nrow(metaData), replace = TRUE))
colsToSub <- "testcol"
start_time <- Sys.time()
isAllNum(metaData_test[which(colnames(metaData_test) == colsToSub)])
end_time <- Sys.time()
print(paste("Original:", end_time - start_time))
start_time <- Sys.time()
isAllNum(metaData_test[colsToSub])
end_time <- Sys.time()
print(paste("New:", end_time - start_time))

# replace all blank values ------------------------------------------------


metaData_test <- cbind(metaData, testcol = rep("", nrow(metaData)))
start_time <- Sys.time()
clinical_data <- metaData_test %>% mutate_all(~ replace(., . == "", NA))
end_time <- Sys.time()
print(paste("Original:", end_time - start_time))
start_time <- Sys.time()
clinical_data2 <- apply(metaData_test, 2, function(x){
  replace(x, x == "", NA)
})
end_time <- Sys.time()
print(paste("New:", end_time - start_time))

# loops and canvassing for plots ------------------------------------------


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

create_plot2 <- function(variable, plot_color, plot_binwidth, title, is_numeric = FALSE) {
  
  #start_time <- Sys.time()
    
  
  if (is_numeric) {
    #p <- ggplot(data = data.frame(measured = as.numeric(as.character(variable))), aes(x = measured)) +
    #  geom_histogram(binwidth = plot_binwidth, fill = plot_color) +
    #  labs(x = "Values",
    #       y = "Frequency") +
    #  ggtitle(title) +
    #  theme_bw(base_size = 18) +
    #  theme(plot.title = element_text(hjust = 0.5))
    
    p <- base_histogram + 
      geom_histogram(data = data.frame(measured = as.numeric(as.character(variable))), aes(x = measured),
                     binwidth = plot_binwidth, fill = plot_color) +
      ggtitle(title)
  }
  else {
    #p <- ggplot(data = as.data.frame(table(variable, useNA = "ifany")), aes(x = variable, y = Freq)) +
    #  geom_bar(stat = "identity", fill = plot_color) +
    #  labs(x = "Values",
    #       y = "Count") +
    #  ggtitle(title) +
    #  scale_x_discrete(labels = sapply(unique(as.character(variable)), shorten_labels, 10)) +
    #  theme_bw(base_size = 18) +
    #  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    #        plot.title = element_text(hjust = 0.5))
    p <- base_barplot +
      geom_bar(data = as.data.frame(table(variable, useNA = "ifany")), aes(x = variable, y = Freq), 
               stat = "identity", fill = plot_color) +
      ggtitle(title) +
      scale_x_discrete(labels = sapply(unique(as.character(variable)), shorten_labels, 10))
  }
  #end_time <- Sys.time()
  #print(paste("Time making", title, "graph", end_time - start_time))
  ggplotly(p) %>% config(displayModeBar = F)
}

color <- "blue"
binwidths <- 30
output <- list()

count <- 0

for (q in 1:10) {
  start_time <- Sys.time()
  plots2 <- lapply(1:ncol(metaData), function(i){
    output[[ make.names(colnames(metaData)[i]) ]] <- 
      suppressWarnings(create_plot(as.character(metaData[,i]), color, binwidths, colnames(metaData)[i], isAllNum(metaData[i])))
  })
  end_time <- Sys.time()
  original <- end_time - start_time
  #print(paste("Original:", end_time - start_time))
  
  start_time <- Sys.time()
  plots2 <- lapply(1:ncol(metaData), function(i){
    output[[ make.names(colnames(metaData)[i]) ]] <- 
      suppressWarnings(create_plot2(as.character(metaData[,i]), color, binwidths, colnames(metaData)[i], isAllNum(metaData[i])))
  })
  end_time <- Sys.time()
  revised <- end_time - start_time
  #print(paste("New:", end_time - start_time))
  
  if (revised > original) {
    count <- count + 1
  }
}
print(count / 10)
  plots2 <- lapply(colnames(metaData), function(i){
    output[[ make.names(i) ]] <- 
      suppressWarnings(create_plot(as.character(metaData[,i]), color, binwidths, i, isAllNum(metaData[i])))
  })

# exclude vars ------------------------------------------------------------

excludeVars <- function(metaData, specs) {
  metaData <- cbind(ID = rownames(metaData), metaData)
  tryCatch({
    for (variable in names(specs)) {
      toExclude <- specs[[variable]]
      metaData <- dplyr::rename(metaData, filter_var = variable)
      if (any(toExclude == "NA")) {
        metaData <- filter(metaData, !is.na(filter_var))
      }
      if (!identical(toExclude, character(0))) {
        for (el in toExclude[which(!toExclude %in% metaData$filter_var)]) {
          if (grepl("exclude", el)) {
            el <- str_split(el, "exclude: ")[[1]][2]
            bounds <- as.numeric(str_split(el, " - ")[[1]])
            metaData <- metaData %>%
              within({
                filter_var <- as.numeric(filter_var)
              }) %>%
              dplyr::filter(filter_var < bounds[1] | filter_var > bounds[2])
          }
          else if (grepl("keep", el)) {
            el <- str_split(el, "keep: ")[[1]][2]
            bounds <- str_split(el, " - ")[[1]]
            #browser()
            metaData <- metaData %>%
              within({
                filter_var <- as.numeric(filter_var)
              }) %>%
              dplyr::filter(filter_var >= bounds[1], filter_var <= bounds[2]) #%>%
            #filter(filter_var <= bounds[2])
          }
        }
        toExclude <- toExclude[which(toExclude %in% metaData$filter_var)]
        metaData <- if (!identical(toExclude, character(0))) filter(metaData, !filter_var %in% toExclude) else metaData
      }
      colnames(metaData)[which(colnames(metaData) == "filter_var")] <- variable
    }
  }, error = function(e) {
    print(e)
    browser()
  })
  rownames(metaData) <- metaData$ID
  metaData <- metaData[-which(colnames(metaData) == "ID")]
  return(metaData)
}

excludeVars2 <- function(metaData, variable, to_exclude) {
  metaData <- cbind(ID = rownames(metaData), metaData)
  tryCatch({
    metaData <- dplyr::rename(metaData, filter_var = variable)
    if (any(to_exclude == "NA")) {
      metaData <- filter(metaData, !is.na(filter_var))
    }
    values <- to_exclude[which(to_exclude %in% metaData$filter_var)]
    metaData <- if (!identical(values, character(0))) filter(metaData, !filter_var %in% values) else metaData
    if (any(!to_exclude %in% metaData$filter_var)) {
      if (grepl("exclude", to_exclude)) {
        el <- str_split(to_exclude, "exclude: ")[[1]][2]
        bounds <- as.numeric(str_split(el, " - ")[[1]])
        metaData <- metaData %>%
          within({
            filter_var <- as.numeric(filter_var)
          }) %>%
          dplyr::filter(filter_var < bounds[1] | filter_var > bounds[2])
      }
      else if (grepl("keep", to_exclude)) {
        el <- str_split(to_exclude, "keep: ")[[1]][2]
        bounds <- as.numeric(str_split(el, " - ")[[1]])
        metaData <- metaData %>%
          within({
            filter_var <- as.numeric(filter_var)
          }) %>%
          dplyr::filter(filter_var >= bounds[1], filter_var <= bounds[2])
      }
    }
    colnames(metaData)[which(colnames(metaData) == "filter_var")] <- variable
  }, error = function(e) {
    print(e)
    browser()
  })
  rownames(metaData) <- metaData$ID
  metaData <- metaData[-which(colnames(metaData) == "ID")]
  return(metaData)
}

test_data <- cbind(metaData, num_col = sample(1:100, nrow(metaData), replace = TRUE))
start_time <- Sys.time()
a <- excludeVars(test_data, list("description" = "sex=m"))
end_time <- Sys.time()
print(paste("Old, alpha:", end_time - start_time))
start_time <- Sys.time()
b <- excludeVars2(test_data, "description", "sex=m")
end_time <- Sys.time()
print(paste("New, alpha:", end_time - start_time))

start_time <- Sys.time()
a <- excludeVars(test_data, list("num_col" = "exclude: 25 - 75"))
end_time <- Sys.time()
print(paste("Old, numeric:", end_time - start_time))
start_time <- Sys.time()
b <- excludeVars2(test_data, "num_col", "exclude: 25 - 75")
end_time <- Sys.time()
print(paste("New, numeric:", end_time - start_time))

# filter expression -------------------------------------------------------

searchStrs <- c(500, 700)
i <- 2

start_time <- Sys.time()
a <- expressionData %>% filter_at(i, any_vars(. >= searchStrs[1] && . <= searchStrs[2]))
end_time <- Sys.time()
print(paste("Old:", end_time - start_time))
start_time <- Sys.time()
b <- expressionData[expressionData[i] >= searchStrs[1] & expressionData[i] <= searchStrs[2],]
end_time <- Sys.time()
print(paste("New:", end_time - start_time))
