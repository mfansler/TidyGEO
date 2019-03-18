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
    }
    values$expression_currChunkLen <- values$expression_currChunkLen + (length(values$expression_oFile) - before)
  }
)

output$expression_downloadRscript <- downloadHandler(
  filename = function() {
    paste0(input$expression_userFileName, ".R")
  },
  content = function(file) {
    #WRITING COMMANDS TO R SCRIPT
    before <- length(values$expression_oFile)
    values$expression_oFile <- saveLines(commentify("save expression data"), values$expression_oFile)
    values$expression_oFile <- saveLines(paste0("file <- ", format_string(input$expression_userFileName)), values$expression_oFile)
    
    if (input$expression_fileType == "csv") {
      values$expression_oFile <- saveLines(paste0("write.csv(expressionData, file, row.names = FALSE)"), values$expression_oFile)
    }
    else if (input$expression_fileType == "tsv") {
      values$expression_oFile <- saveLines("write.table(expressionData, file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)", 
                                           values$expression_oFile)
    }
    else if (input$expression_fileType == "JSON") {
      values$oFile <- saveLines(c("library(jsonlite)", "library(readr)", 
                                  "expressionData %>% toJSON() %>% write_lines(file)"), 
                                values$expression_oFile)
    }
    else if (input$expression_fileType == "xlsx") {
      values$expression_oFile <- saveLines(c("library(xlsx)", "write.xlsx(expressionData, file, row.names = FALSE, showNA = FALSE)"), 
                                           values$expression_oFile)
    }
    
    saveToRscript(values$expression_oFile, file, 'User/assay_helper_functions.R')
    
    values$expression_currChunkLen <- values$expression_currChunkLen + (length(values$expression_oFile) - before)
  }
)