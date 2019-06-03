output$expression_nameFile <- renderUI({
  textInput("expression_userFileName", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = paste0(input$geoID, "_Data.", input$expression_fileType))
})

output$expression_downloadData <- downloadHandler(
  filename = function() {
    input$expression_userFileName
  },
  content = function(file) {
    
    #values$exprToDisplay <- filterExpressionData(values$exprToDisplay, input$exprPreview_search_columns)
    
    if (input$expression_fileType == "csv") {
      
      #print("exprToDisplay")
      #print(values$exprToDisplay, n = 10)
      withProgress(message = "Writing data to file", {
        incProgress()
        write.csv(assay_vals$assay_data, file, row.names = FALSE)
        incProgress()
      })
    }
    else if (input$expression_fileType == "tsv") {
      withProgress(message = "Writing data to file",
                   write.table(assay_vals$assay_data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
    }
    else if (input$expression_fileType == "JSON") {
      library(jsonlite)
      #library(readr)
      
      withProgress(message = "Writing data to file", {
        incProgress()
        assay_vals$assay_data %>% toJSON() %>% write_lines(file)
        incProgress()
      })
      
    }
    else if (input$expression_fileType == "xlsx") {
      library(xlsx)
      
      withProgress(message = "Writing data to file", {
        incProgress()
        write.xlsx(assay_vals$assay_data, file, row.names = FALSE, showNA = FALSE)
        incProgress()
      })
    } else {
      files <- c(paste0(tempfile(), input$geoID, "_Data.txt"), paste0(tempfile(), input$geoID, "_GeneAnnotations.txt"))
      withProgress(message = "Writing data to file",
                   write.table(assay_vals$assay_data, files[1], sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
      withProgress(message = "Writing feature data to file",
                   write.table(assay_vals$feature_data, files[2], sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
      zip(file, files)
    }
  }
)

output$expression_downloadRscript <- downloadHandler(
  filename = function() {
    paste0(input$expression_userFileName, ".R")
  },
  content = function(file) {
    #WRITING COMMANDS TO R SCRIPT
    before <- length(assay_vals$oFile)
    assay_vals$oFile <- saveLines(commentify("save expression data"), assay_vals$oFile)
    assay_vals$oFile <- saveLines(paste0("file <- ", format_string(input$expression_userFileName)), assay_vals$oFile)
    
    if (input$expression_fileType == "csv") {
      assay_vals$oFile <- saveLines(paste0("write.csv(expressionData, file, row.names = FALSE)"), assay_vals$oFile)
    }
    else if (input$expression_fileType == "tsv") {
      assay_vals$oFile <- saveLines("write.table(expressionData, file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)", 
                                           assay_vals$oFile)
    }
    else if (input$expression_fileType == "JSON") {
      assay_vals$oFile <- saveLines(c("library(jsonlite)", "library(readr)", 
                                  "expressionData %>% toJSON() %>% write_lines(file)"), 
                                assay_vals$oFile)
    }
    else if (input$expression_fileType == "xlsx") {
      assay_vals$oFile <- saveLines(c("library(xlsx)", "write.xlsx(expressionData, file, row.names = FALSE, showNA = FALSE)"), 
                                           assay_vals$oFile)
    }
    
    saveToRscript(assay_vals$oFile, version, file, 'User/assay_helper_functions.R')
    
    assay_vals$current_chunk_len <- assay_vals$curr_chunk_len + (length(assay_vals$oFile) - before)
  }
)