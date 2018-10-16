library(tidyverse)
GSE1456_Clinical_Raw <- read_delim("GEOcurate/User/GSE1456_Clinical_Raw.txt",
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
GSE1456 <- GSE1456_Clinical_Raw %>% 
  select(SampleID, characteristics_ch1.1, characteristics_ch1.2, characteristics_ch1.3, relation)
GSE1456 <- as.data.frame(GSE1456)
GSE1456_2 <- extractColNames(GSE1456, ": ", c("characteristics_ch1.1", "characteristics_ch1.2", "characteristics_ch1.3"))

excludeVars <- function(metaData, specs) {
  for (variable in names(specs)) {
    toExclude <- specs[[variable]]
    if (any(toExclude == "NA") && any(is.na(metaData[,variable]))) {
      toExclude <- toExclude[-which(toExclude == "NA")]
      metaData <- metaData[-which(is.na(metaData[,variable])),]
    }
    if (!identical(toExclude, character(0))) {
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
        metaData[,variable] <- as.numeric(metaData[,variable])
      }
      toExclude <- toExclude[which(toExclude %in% metaData[,variable])]
      metaData <- if (!identical(toExclude, character(0))) metaData[which(!(metaData[,variable] %in% toExclude)),] else metaData
    }
  }
  return(metaData)
}

excludeVars2 <- function(metaData, specs) {
  for (variable in names(specs)) {
    toExclude <- specs[[variable]]
    metaData <- rename(metaData, workingCol = variable)
    if (any(toExclude == "NA") && any(is.na(metaData[,variable]))) {
      toExclude <- toExclude[-which(toExclude == "NA")]
      #metaData <- metaData[-which(is.na(metaData[,variable])),]
      metaData <- filter(metaData, !is.na(workingCol))
    }
    if (!identical(toExclude, character(0))) {
      for(el in toExclude[which(!toExclude %in% metaData$workingCol)]) {
        if (grepl("exclude", el)) {
          el <- str_split(el, "exclude: ")[[1]][2]
          bounds <- as.numeric(str_split(el, "-")[[1]])
          
          #indices <- sapply(metaData[,variable], 
          #                  function(x){
          #                    if (bounds[1] <= x && x <= bounds[2]) FALSE else TRUE
          #                  })
          #metaData <- metaData[which(indices),]
          metaData <- filter(metaData, bounds[1] <= workingCol && workingCol <= bounds[2])
        }
        else if (grepl("keep", el)) {
          el <- str_split(el, "keep: ")[[1]][2]
          bounds <- as.numeric(str_split(el, "-")[[1]])
          #metaData <- metaData[which(as.numeric(metaData[,variable]) >= bounds[1]),]
          #metaData <- metaData[which(as.numeric(metaData[,variable]) <= bounds[2]),]
          metaData <- filter(metaData, workingCol >= bounds[1], workingCol <= bounds[2])
        }
        metaData$workingCol <- as.numeric(metaData$workingCol)
      }
      toExclude <- toExclude[which(toExclude %in% metaData$workingCol)]
      metaData <- if (!identical(toExclude, character(0))) filter(metaData, !(workingCol %in% toExclude)) else metaData
    }
    colnames(metaData)[which(colnames(metaData) == "workingCol")] <- variable
  }
  return(metaData)
}
start_time <- Sys.time()
old_data <- excludeVars(GSE1456, list("characteristics_ch1.3" = "DEATH: 1"))
end_time <- Sys.time()
old_time <- end_time - start_time
start_time <- Sys.time()
new_data <- excludeVars2(GSE1456, list("characteristics_ch1.3" = "DEATH: 1"))
end_time <- Sys.time()
new_time <- end_time - start_time
print("Old time:")
print(old_time)
print("New time:")
print(new_time)

start_time <- Sys.time()
old_data <- excludeVars(GSE1456_2, list("SURV_RELAPSE" = "keep: 3-4"))
end_time <- Sys.time()
old_time <- end_time - start_time
start_time <- Sys.time()
new_data <- excludeVars2(GSE1456_2, list("SURV_RELAPSE" = "keep: 3-4"))
end_time <- Sys.time()
new_time <- end_time - start_time
print("Old time:")
print(old_time)
print("New time:")
print(new_time)