library(tidyverse)
GSE1456_Clinical_Raw <- read_delim("GEOcurate/User/GSE1456_Clinical_Raw.txt",
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
GSE1456 <- GSE1456_Clinical_Raw %>% 
  select(SampleID, characteristics_ch1.1, characteristics_ch1.2, characteristics_ch1.3, relation)
GSE1456 <- as.data.frame(GSE1456)

filterCols <- function(metaData, varsToKeep, allButKeep) {
  
  if (allButKeep) {
    
    metaData <- if(!is.null(varsToKeep)) metaData[-which(colnames(metaData) %in% varsToKeep)] else metaData
    
  }
  else {
    #varsToKeep <- c(varsToKeep, "evalSame")
    metaData <- metaData[which(colnames(metaData) %in% varsToKeep)]
  }
  return(metaData)
}

filterCols2 <- function(metaData, varsToKeep, allButKeep) {
  
  if (allButKeep) {
    
    metaData <- if(!is.null(varsToKeep)) select(metaData, -varsToKeep) else metaData
    
  }
  else {
    #varsToKeep <- c(varsToKeep, "evalSame")
    metaData <- select(metaData, varsToKeep)
  }
  return(metaData)
}

start_time <- Sys.time()
old_data <- filterCols(GSE1456, "characteristics_ch1.3", TRUE)
end_time <- Sys.time()
old_filter <- end_time - start_time
start_time <- Sys.time()
new_data <- filterCols2(GSE1456, "characteristics_ch1.3", TRUE)
end_time <- Sys.time()
new_filter <- end_time - start_time
print("Old time:")
print(old_filter)
print("New time:")
print(new_filter)
