output$feature_nameFile <- renderUI({
  textInput("feature_userFileName", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = paste0(input$geoID, "_Features.", input$feature_fileType))
})

output$feature_downloadData <- downloadHandler(
  filename = function() {
    input$feature_userFileName
  },
  content = function(file) {
    
    #values$exprToDisplay <- filterfeatureData(values$exprToDisplay, input$exprPreview_search_columns)
    
    if (input$feature_fileType == "csv") {
      
      #print("exprToDisplay")
      #print(values$exprToDisplay, n = 10)
      withProgress(message = "Writing data to file", {
        incProgress()
        write.csv(feature_vals$feature_data, file, row.names = FALSE)
        incProgress()
      })
    }
    else if (input$feature_fileType == "tsv") {
      withProgress(message = "Writing data to file",
                   write.table(feature_vals$feature_data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
    }
    else if (input$feature_fileType == "JSON") {
      library(jsonlite)
      #library(readr)
      
      withProgress(message = "Writing data to file", {
        incProgress()
        feature_vals$feature_data %>% toJSON() %>% write_lines(file)
        incProgress()
      })
      
    }
    else if (input$feature_fileType == "xlsx") {
      library(xlsx)
      
      withProgress(message = "Writing data to file", {
        incProgress()
        write.xlsx(feature_vals$feature_data, file, row.names = FALSE, showNA = FALSE)
        incProgress()
      })
    } else {
      myData <- feature_vals$feature_data
      colnames(myData) <- colnames(myData) <- str_replace_all(colnames(myData), "[\\\\\\/:\\*\\?\\<\\>\\=\\+\\#\\~\\`\\'\\;\\&\\%\\$\\@\\!]", "_")
    
      withProgress(message = "Writing data to file",
                   write.table(feature_vals$feature_data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
    }
  }
)

output$feature_downloadRscript <- downloadHandler(
  filename = function() {
    paste0(input$feature_userFileName, ".R")
  },
  content = function(file) {
    #WRITING COMMANDS TO R SCRIPT
    before <- length(feature_vals$oFile)
    feature_vals$oFile <- saveLines(commentify("save feature data"), feature_vals$oFile)
    feature_vals$oFile <- saveLines(paste0("file <- ", format_string(input$feature_userFileName)), feature_vals$oFile)
    
    if (input$feature_fileType == "csv") {
      feature_vals$oFile <- saveLines(paste0("write.csv(featureData, file, row.names = FALSE)"), feature_vals$oFile)
    }
    else if (input$feature_fileType == "tsv") {
      feature_vals$oFile <- saveLines("write.table(featureData, file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)", 
                                    feature_vals$oFile)
    }
    else if (input$feature_fileType == "JSON") {
      feature_vals$oFile <- saveLines(c("library(jsonlite)", "library(readr)", 
                                      "featureData %>% toJSON() %>% write_lines(file)"), 
                                    feature_vals$oFile)
    }
    else if (input$feature_fileType == "xlsx") {
      feature_vals$oFile <- saveLines(c("library(xlsx)", "write.xlsx(featureData, file, row.names = FALSE, showNA = FALSE)"), 
                                    feature_vals$oFile)
    }
    
    saveToRscript(feature_vals$oFile, version, file, 'User/assay_helper_functions.R')
    
    feature_vals$current_chunk_len <- feature_vals$current_chunk_len + (length(feature_vals$oFile) - before)
  }
)