output$clinical_display_filename <- renderUI({
  textInput("clinical_user_filename", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
            value = paste0(input$geoID, "_Annotations.", input$clinical_file_type))
})

output$clinical_evaluate_save <- downloadHandler(
  filename = function() {
    input$clinical_user_filename
  },
  content = function(file) {
    myData <- clinical_vals$clinical_data
    #myData <- myData[-which(grepl("evalSame", colnames(myData)))]
    myData <- cbind(rownames(myData), myData)
    colnames(myData)[1] <- ""
    
    if (input$clinical_file_type == "csv") {
      write.csv(myData, file, row.names = FALSE)
    }
    else if (input$clinical_file_type == "tsv") {
      write.table(myData, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
    }
    else if (input$clinical_file_type == "JSON") {
      library(jsonlite)
      #library(readr)
      
      myData %>% toJSON() %>% write_lines(file)
    }
    else if (input$clinical_file_type == "xlsx") {
      library(xlsx)
      
      write.xlsx(myData, file, row.names = FALSE, showNA = FALSE)
    } else {
      colnames(myData)[1] <- "ExpId"
      colnames(myData) <- str_replace_all(colnames(myData), "[\\\\\\/:\\*\\?\\<\\>\\=\\+\\#\\~\\`\\'\\;\\&\\%\\$\\@\\!]", "_")
      write.table(myData, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
    }
  }
)

output$clinical_save_rscript <- downloadHandler(
  filename = function() {
    paste0(input$clinical_user_filename, ".R")
  },
  content = function(file) {
    #WRITING COMMANDS TO R SCRIPT
    before <- length(clinical_vals$oFile)
    clinical_vals$oFile <- saveLines(commentify("save data"), clinical_vals$oFile)
    clinical_vals$oFile <- saveLines(c("clinical_data <- cbind(rownames(clinical_data), clinical_data)", 
                                "colnames(clinical_data)[1] <- ''", 
                                paste0("file <- ", format_string(input$clinical_user_filename))), clinical_vals$oFile)
    
    if (input$clinical_file_type == "csv") {
      clinical_vals$oFile <- saveLines(paste0("write.csv(clinical_data, file, row.names = FALSE)"), clinical_vals$oFile)
    }
    else if (input$clinical_file_type == "tsv") {
      clinical_vals$oFile <- saveLines("write.table(clinical_data, file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)", 
                                clinical_vals$oFile)
    }
    else if (input$clinical_file_type == "JSON") {
      clinical_vals$oFile <- saveLines(c("library(jsonlite)", "library(readr)", 
                                  "clinical_data %>% toJSON() %>% write_lines(file)"), 
                                clinical_vals$oFile)
    }
    else if (input$clinical_file_type == "xlsx") {
      clinical_vals$oFile <- saveLines(c("library(xlsx)", "write.xlsx(clinical_data, file, row.names = FALSE, showNA = FALSE)"), 
                                clinical_vals$oFile)
    } else {
      
    }
    
    clinical_vals$current_chunk_len <- clinical_vals$current_chunk_len + (length(clinical_vals$oFile) - before)
    
    saveToRscript(clinical_vals$oFile, version, file)
  }
)