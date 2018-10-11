library(readr)
GSE1456_Clinical_Raw <- read_delim("GSE1456_Clinical_Raw.txt",
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
View(GSE1456_Clinical_Raw)

GSE1456 <- GSE1456_Clinical_Raw %>% select(SampleID, characteristics_ch1.1, characteristics_ch1.2, characteristics_ch1.3, relation)

GSE1456_split <- separate(GSE1456, "SampleID", sep = ": ", into = c("key", "value"))

GSE1456_gather <- spread(GSE1456_split, key = "key", value = "value")

GSE1456_test <- rbind(GSE1456, rep("testval=testVal", ncol(GSE1456)))

extractColNames2 <- function(inputDataFrame, delimiter, colsToSplit) {
  for(col in colsToSplit) {
    if(all(grepl(delimiter, inputDataFrame[which(!is.na(inputDataFrame[,col])),col]))) {
      inputDataFrame <- separate(inputDataFrame, col, sep = delimiter, into = c("key", "value"))
      inputDataFrame <- spread(inputDataFrame, key = "key", value = "value")
    }
  }
  return(inputDataFrame)
}

all(grepl(": ", GSE1456_test[which(!is.na(GSE1456_test[,"characteristics_ch1.2"])),"characteristics_ch1.2"]))

View(extractColNames(GSE1456_test, c("characteristics_ch1.2", ": ")))
View(extractColNames2(GSE1456_test, ": ", "characteristics_ch1.2"))

extractColNames <- function(inputDataFrame, delimiterInfo)
{
  classAndClinical <- inputDataFrame
  prefixes = NULL
  
  for (i in 1:nrow(classAndClinical))
  {
    for (j in 1:ncol(classAndClinical))
    {
      value <- as.character(classAndClinical[i,j])
      pattern <- delimiterInfo[which(delimiterInfo == colnames(classAndClinical[j]))+1]
      
      if (colnames(classAndClinical)[j] %in% delimiterInfo && grepl(pattern, value)) {
        
        prefix <- str_split(value, pattern)[[1]][1]
        prefix <- str_trim(prefix)
        
      }
      else {
        prefix <- colnames(classAndClinical)[j]
      }
      
      if (prefix != "")
        prefixes <- c(prefixes, prefix)
    }
  }
  
  prefixes <- sort(unique(prefixes))
  
  classAndClinical2 <- data.frame(rownames(classAndClinical))
  
  for (prefix in prefixes)
  {
    rowValues = NULL
    
    for (i in 1:nrow(classAndClinical))
    {
      rowValue = "?"
      
      for (j in 1:ncol(classAndClinical))
      {
        value <- as.character(classAndClinical[i,j])
        pattern <- delimiterInfo[which(delimiterInfo == colnames(classAndClinical[j]))+1]
        
        if (colnames(classAndClinical)[j] %in% delimiterInfo && grepl(pattern, value)) {
          valueParts <- str_split(value, pattern)[[1]]
          thisPrefix <- str_trim(valueParts[1])
          thisValue <- valueParts[2]
        }
        
        else
        {
          thisPrefix <- colnames(classAndClinical)[j]
          thisValue <- value
        }
        
        if (thisPrefix == prefix) {
          thisValue = str_trim(thisValue)
          rowValue = thisValue
        }
      }
      
      rowValues <- c(rowValues, rowValue)
    }
    
    classAndClinical2 <- cbind(classAndClinical2, rowValues)
  }
  
  prefixesNew <- c("SampleID", str_replace_all(prefixes, " ", "_"))
  prefixesNew <- str_replace_all(prefixesNew,"\\(","")
  prefixesNew <- str_replace_all(prefixesNew,"\\)","")
  
  colnames(classAndClinical2) <- prefixesNew
  
  rownames(classAndClinical2) <- as.vector(classAndClinical2[,1])
  classAndClinical2 <- classAndClinical2[,-1]
  
  classAndClinical2 <- filterUninformativeCols(classAndClinical2, list("none"))
  
  return(classAndClinical2)
}
