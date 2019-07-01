output$feature_nameFile <- renderUI({
  textInput("feature_userFileName", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = paste0(input$geoID, "_Data.", input$feature_fileType))
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
        write.csv(assay_vals$feature_data, file, row.names = FALSE)
        incProgress()
      })
    }
    else if (input$feature_fileType == "tsv") {
      withProgress(message = "Writing data to file",
                   write.table(assay_vals$feature_data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
    }
    else if (input$feature_fileType == "JSON") {
      library(jsonlite)
      #library(readr)
      
      withProgress(message = "Writing data to file", {
        incProgress()
        assay_vals$assay_data %>% toJSON() %>% write_lines(file)
        incProgress()
      })
      
    }
    else if (input$feature_fileType == "xlsx") {
      library(xlsx)
      
      withProgress(message = "Writing data to file", {
        incProgress()
        write.xlsx(assay_vals$feature_data, file, row.names = FALSE, showNA = FALSE)
        incProgress()
      })
    } else {
      myData <- assay_vals$feature_data
      colnames(myData) <- colnames(myData) <- str_replace_all(colnames(myData), "[\\\\\\/:\\*\\?\\<\\>\\=\\+\\#\\~\\`\\'\\;\\&\\%\\$\\@\\!]", "_")
    
      withProgress(message = "Writing data to file",
                   write.table(assay_vals$feature_data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
    }
  }
)

output$feature_downloadRscript <- downloadHandler(
  filename = function() {
    paste0(input$feature_userFileName, ".R")
  },
  content = function(file) {
    #WRITING COMMANDS TO R SCRIPT
    before <- length(assay_vals$oFile)
    assay_vals$oFile <- saveLines(commentify("save feature data"), assay_vals$oFile)
    assay_vals$oFile <- saveLines(paste0("file <- ", format_string(input$feature_userFileName)), assay_vals$oFile)
    
    if (input$feature_fileType == "csv") {
      assay_vals$oFile <- saveLines(paste0("write.csv(featureData, file, row.names = FALSE)"), assay_vals$oFile)
    }
    else if (input$feature_fileType == "tsv") {
      assay_vals$oFile <- saveLines("write.table(featureData, file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)", 
                                    assay_vals$oFile)
    }
    else if (input$feature_fileType == "JSON") {
      assay_vals$oFile <- saveLines(c("library(jsonlite)", "library(readr)", 
                                      "featureData %>% toJSON() %>% write_lines(file)"), 
                                    assay_vals$oFile)
    }
    else if (input$feature_fileType == "xlsx") {
      assay_vals$oFile <- saveLines(c("library(xlsx)", "write.xlsx(featureData, file, row.names = FALSE, showNA = FALSE)"), 
                                    assay_vals$oFile)
    }
    
    saveToRscript(assay_vals$oFile, version, file, 'User/assay_helper_functions.R')
    
    assay_vals$current_chunk_len <- assay_vals$curr_chunk_len + (length(assay_vals$oFile) - before)
  }
)