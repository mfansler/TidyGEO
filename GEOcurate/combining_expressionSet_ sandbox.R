metaData1
metaData2

all(colnames(metaData1) %in% colnames(metaData2))

setdiff(colnames(metaData1), colnames(metaData2))

diffCols <- setdiff(colnames(metaData1), colnames(metaData1))

diffCols <- list("metaData1" = which(!colnames(metaData1) %in% colnames(metaData2)),
              "metaData2" = which(!colnames(metaData2) %in% colnames(metaData1)))

if(identical(diffCols[[1]], integer(0)) || identical(diffCols[[2]])) {
  #houston, we have a problem
  
}

metaDatas <- NULL

for(i in 1:length(expressionSet)) {
  metaDatas[[i]] <- pData(expressionSet[[i]])
}

allMetaData <- metaDatas[[1]]

if(length(metaDatas) > 1) {
  
  for (element in metaDatas) {
    status <- tryCatch({
      allMetaData <- rbind(allMetaData, element)
      "pass"
    }, error = function(e) {
      #determine which column is the bad one
      diffCols <- setdiff(colnames(allMetaData), colnames(element))
      diffsFromAll <- which(!colnames(allMetaData) %in% colnames(element))
      diffsFromCurrent <- which(!colnames(element) %in% colnames(allMetaData))
      
      if(str_detect(e, "names do not match previous names")) {
        
      } else if (str_detect(e, "numbers of columns of arguments do not match")) {
        
      }
      "fail"
    }
    )
  }
  
  #idea:
  #have a pop-up that shows two heads, one for allData and one for the dataset you're trying to add.
  #the columns that are different should be highlighted.
  #Two options: either don't merge the datasets and keep the first ones that were merged successfully, or
  #Choose to keep some columns. Any columns that are not kept will be dropped
  #In the for loop, make the changes to the datasets so you can rbind them successfully
  #Continue with the for loop
  #
  #use a modal https://shiny.rstudio.com/reference/shiny/0.14/modalDialog.html
  #figure out how to pause the for loop while the modal is open (maybe use a while loop that doesn't do anything while a
  #certain input is NULL??)
  
}