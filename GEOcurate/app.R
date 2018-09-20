library(shiny)
library(DT)
library(shinycssloaders)
library(stringr)
library(shinyBS)
library(rhandsontable)
library(shinyjs)
library(feather)
library(shinysky)
library(shinyFiles)
library(tidyr)
source("geocurateFunctions.R")



# help icon to add as tag to buttons, etc ---------------------------------


helpButton <- function(message = "content", placement = "right") {
  return(tipify(icon("question-circle"), title = message, placement = placement, trigger = "hover"))
}

# detects variable type & formats string to be written to R script --------


formatString <- function(el) {
  if(is.null(el)) {
    return("NULL")
  }
  else if(is.na(el)) {
    return("NA")
  }
  else if (typeof(el) == "double") {
    for (i in 1:length(el)) {
      el[i] <- as.character(el[i])
    }
    return(el)
  }
  else if (typeof(el) == "character") {
    for (i in 1:length(el)) {
      el[i] <- paste0("'", el[i], "'")
    }
    return(el)
  }
  else if (el == TRUE) {
    return("TRUE")
  }
  else if (el == FALSE) {
    return("FALSE")
  }
}

# creates section headings for R script -----------------------------------


commentify <- function(myStr) {
  
  numChars <- 75
  comment <- paste0("# ", myStr, " ")
  comment <- paste0(comment, paste(rep("-", numChars - nchar(comment)), collapse = ""))
  return(c("", "", comment, ""))
}

# download thesaurus files from working directory -------------------------


thesaurus.synonyms.feather <- read_feather("thesaurus.synonyms.feather")
thesaurus.preferred.feather <- read_feather("thesaurus.preferred.feather")

options(shiny.autoreload = F)

# UI ----------------------------------------------------------------------


ui <- fluidPage(
  
  tabsetPanel(id = "masterPanel",
    tabPanel("metaData",
  
      sidebarLayout(
        
        sidebarPanel(
          useShinyjs(),
          tabsetPanel(id = "sidePanel",
                      
                      
    # download data -----------------------------------------------------------
    
    
            tabPanel("1",
              h4("Downloading the data"),
              textInput(inputId = "geoID", label = div("Please input a GSE ID: ", 
                                                       helpButton('The GEO identifier for the dataset, e.g., "GSE1456"'))),
              checkboxGroupInput(inputId = "filter", label = div("Would you like to filter any of the following?", 
                                                                   helpButton("Removes columns right after downloading, according to the following specifications.")),
                                   choiceNames = list(div("All the same value", helpButton("Columns in which every value is the same are often uninformative.")), 
                                                      div("All different values", helpButton("Columns in which every value is different are often uninformative.")), 
                                                      div("Dates", helpButton("Removes columns in which every entry is in the format January 00 00 (for example).")),
                                                      div("Reanalyzed by", helpButton('Removes columns in which every entry contains the phrase "reanalyzed by", which are often uninformative.')), 
                                                      div("Web addresses", helpButton('Removes columns in which every entry is a web address beginning with "ftp:://"'))),
                                   choiceValues = list("sameVals", "allDiff", "dates", "reanalyzed", "url")),
              checkboxInput(inputId = "alsoDownload", label = div("Also download series matrix file,",
                                                                       helpButton("Files to download in addition to the metadata file."))),
              actionButton(inputId = "download", label = "Download"),
              hr(), uiOutput("nav_1_ui")
            ),
    
    
    # split variables ---------------------------------------------------------
    
            
            #specify which columns to split up, and the delimiter
            tabPanel("2",
              #hr(),
              h4("Formatting the data"),
              checkboxInput(inputId = "toSplit", label = div("Contains key-value pairs separated by a delimiter",
                                                               helpButton('e.g. <b>"RELAPSE: 1"</b>'))),
              conditionalPanel(condition = "input.toSplit == true",
                        uiOutput("chooseSplitVars"),
                        checkboxInput(inputId = "allButSplit", label = tags$i("split all BUT the specified")),
                        textInput(inputId = "delimiter", label = "Delimiter (including any spaces): ")
                        ),
              checkboxInput(inputId = "toDivide", label = div("Contains multiple values in one column",
                                                              helpButton('e.g. <b>"responder;7;control"</b>'))),
              conditionalPanel(condition = "input.toDivide == true",
                               uiOutput("chooseDivideVars"),
                               checkboxInput(inputId = "allButDivide", label = tags$i("split all BUT the specified")),
                               textInput(inputId = "delimiter2", label = "Delimiter (including any spaces): ")
                               #uiOutput("chooseNewDivideNames")
                               #textOutput("variable1")
                               ),
              #specify which values should be excluded from the column
              #option to save the specification files
              #thesaurus stuff 
              actionButton(inputId = "extractCols", label = "Extract columns"),
              hr(), uiOutput("nav_2_ui")
            ),
    
    # exclude columns ---------------------------------------------------------
    
            
            #specify which vars to keep
            tabPanel("3",
                     #hr(),
                     #h4("Choosing the variables"),
                     uiOutput("chooseVarsToKeep"),
                     checkboxInput(inputId = "allButKeep", label = tags$i("keep all BUT the specified")),
                     actionButton(inputId = "filterCols", label = "Filter columns"),
                     hr(), uiOutput("nav_3_ui")
            ),
    
    # rename columns ----------------------------------------------------------
    
            
            #renaming any columns
            tabPanel("4",
                     uiOutput("showCols"),
                     #textInput("newName", "Enter a new name: "),
                     rHandsontableOutput("newName"),
                     uiOutput("thesLink"),
                     #textInput.typeahead(id="newName",
                      #                   placeholder="Enter a new name: ",
                      #                   local=data.frame(thesaurus.preferred.feather),
                      #                   valueKey = "Code",
                      #                   tokens=c(1:nrow(thesaurus.preferred.feather)),
                      #                   template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
                     #),
                     actionButton("save", "Save"),
                     actionButton("delete", "Remove"), br(),
                     #actionButton("addPrefName", "Add name"),
                     #actionButton('removeBtn', 'Remove name'),
                     br(), HTML("<p><b>Here are the new names you have specified so far: </b></p>"),
                     tableOutput("new_name_contents"),
                     #verbatimTextOutput("testPrint"),
                     actionButton(inputId = "rename", label = "Rename columns"),
                     hr(), uiOutput("nav_4_ui")
                     ),
    
    # substitute --------------------------------------------------------------
    
            
            #specify which values to substitute for other values/which values should be treated as NA
            tabPanel("5",
                     #hr(),
                     #h4("Cleaning the data"),
                     uiOutput("showColsForSub"),
                     #actionButton("saveNA", "Save"),
                     #actionButton('removeNA', 'Remove'),
                     #br(), textOutput("curr_NA_vals"), br(),
                     checkboxInput("isRange", "Specify a range of values to substitute?"),
                     conditionalPanel(condition = "input.isRange == true",
                                                       uiOutput("sliderSub")),
                     conditionalPanel(condition = "input.isRange == false",
                                      h5('Click "Add" to add rows or "Remove" to remove the last row.'),
                                      rHandsontableOutput("hotIn")),
                     uiOutput("thesLinkSub"),
                     actionButton("addToSub", "Add"),
                     actionButton("removeToSub", "Remove"),
                     div(#style = 'overflow-x: scroll', 
                       DTOutput("hotOut")),
                     #uiOutput("hotOutText"),
                     #rhandsontable(data.frame(To_Replace = "", New_Val = "", stringsAsFactors = FALSE), width = 250, height = 100, stretchH = "all"),
                     actionButton("evaluateSubs", "Substitute"),
                     hr(), uiOutput("nav_5_ui")
            ),
    
    # exclude variables -------------------------------------------------------
    
    
            tabPanel("6",
                     uiOutput("showColsForExclude"),
                     checkboxInput("isRangeExclude", "Specify a range of values"),
                     conditionalPanel(condition = "input.isRangeExclude == true",
                                      uiOutput("sliderExclude")),
                     conditionalPanel(condition = "input.isRangeExclude == false",
                                      uiOutput("showValsForExclude")),
                     actionButton("excludeVals", "Exclude"),
                     hr(), uiOutput("nav_6_ui")
            )
          )
        ),
    
    # display metadata --------------------------------------------------------
    
        
        mainPanel(
          tabsetPanel(
            tabPanel("MetaData", 
                     br(), 
                     fluidRow(
                       #option to save the output file
                     column(2, actionButton("reset", "Reset")),
                     column(2, actionButton("undo", "Undo"), helpButton("Reset the dataset to its original state or undo the last action.")),
                     #warning about merged datasets using dates as the criteria
                     column(6, offset = 2, uiOutput("mergedWarning"))
                     ), 
                     br(), br(), 
                     bsAlert("alert"), withSpinner(DTOutput("dataset"), type = 5)
                     ),
            tabPanel("Summary", #verbatimTextOutput("metaSummary")
                     
                     uiOutput("plots")
                     ),
            tabPanel("Save file",
                     radioButtons("fileType", "File type:", choices = c("csv", "tsv", "JSON")),
                     uiOutput("nameFile"),
                     fluidRow(
                      column(2, downloadButton("downloadData", "Save")),
                      column(2, offset = 0, downloadButton("downloadRscript", "Download R script"))
                     )
                     #,
                     #actionButton("showHistory", "Show History"),
                     )
            ) #tab panel in main panel
        ) #main panel
      ) #sidebar layout
    ), #metadata tab panel

# expression data ---------------------------------------------------------

    tabPanel("Series data",
             sidebarLayout(
               sidebarPanel(
                 #actionButton(inputId = "transposeExpr", label = "Transpose")
                 uiOutput("exprLabels"),
                 uiOutput("summarizeOptions"),
                 checkboxInput("transposeExpr", label = "Transpose the data"),
                 actionButton(inputId = "undoEvalExpr", label = "Undo"),
                 actionButton(inputId = "previewExpr", label = "Preview")
                 ),
               mainPanel(
                 withSpinner(DTOutput("expressionData"), type = 5),
                 withSpinner(DTOutput("featureData"), type = 5)
                 ) #main panel
               ) #sidebar layout
             ) # expression data tab panel
  ) #master panel
) #fluidPage

# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  #look for setting to get it do not disconnect
  #session$allowReconnect(TRUE)
  
  values <-
    reactiveValues(
      metaData = NULL,
      origData = NULL,
      lastData = NULL,
      exprData = NULL,
      exprToDisplay = NULL,
      lastExprToDisplay = NULL,
      ftData = NULL,
      ftToDisplay = NULL,
      newName = NULL,
      newNames = NULL,
      NAvalsList = list(),
      DFIn = data.frame(),
      DFOut = data.frame(
        To_Replace = "",
        New_Val = "",
        stringsAsFactors = FALSE
      ),
      tablesList = list(),
      thes_suggest = c("no suggestions"),
      thes_suggest_vals = c("no suggestions"),
      suggestions = c("no suggestions"),
      excludesList = list(),
      oFile = "source('geocurateFunctions_User.R')",
      downloadChunkLen = 0,
      currChunkLen = 0,
      subAllNums = F
    )

# reset -------------------------------------------------------------------

observeEvent(input$reset, {
  values$metaData <- values$origData
  values$oFile <- removeFromScript(values$oFile, len = values$downloadChunkLen, all = T)
  values$currChunkLen <- 0
})  

# Undo --------------------------------------------------------------------

observeEvent(input$undo, {
  values$metaData <- values$lastData
  #print(values$currChunkLen)
  values$oFile <- removeFromScript(values$oFile, len = values$currChunkLen)
  values$currChunkLen <- 0
})  


# download & display metaData ---------------------------------------------
  
  observeEvent(input$download, {
    if (input$geoID == "") {
      values$errorState <- TRUE
      createAlert(session, "alert", "inputError", title = "Error",
                  content = "Please specify a GSE ID", append = FALSE)
    }
    else {
      closeAlert(session, "inputError")
      closeAlert(session, "fileError")
      values$errorState <- FALSE
      allData <- withProgress(downloadClinical(input$geoID, input$filter, session = session, 
                                               downloadExpr = input$alsoDownload), 
                              message = "Downloading data")
      
      values$metaData <- allData[["metaData"]]
      values$exprData <- allData[["expressionData"]]
      values$exprToDisplay <- head(values$exprData, n = 10)[,1:5]
      values$lastExprToDisplay <- values$exprToDisplay
      values$ftData <- allData[["featureData"]]
      print(which(values$ftData[,"ID"] %in% values$exprToDisplay[,"ID"]))
      values$ftToDisplay <- values$ftData[which(values$ftData[,"ID"] %in% values$exprToDisplay[,"ID"]),]
      
      #WRITING COMMANDS TO R SCRIPT
      values$oFile <- "source('geocurateFunctions_User.R')"
      values$oFile <- saveLines(commentify("download metaData"), values$oFile)
      values$oFile <- saveLines(paste0("toFilter <- NULL"), values$oFile)
      for (i in 1:length(input$filter)) {
        values$oFile <- saveLines(paste0("toFilter[", i, "] <- ", formatString(input$filter[i])), values$oFile)
      }
      values$oFile <- saveLines(c(paste0("geoID <- ", formatString(input$geoID)), "metaData <- downloadClinical(geoID, toFilter)"), values$oFile)
      downloadChunkLen <- length(values$oFile)
      
      values$origData <- values$metaData
      #values$lastData <- values$metaData
    }
    })
  
  output$dataset <- DT::renderDT({
    if(!is.null(values$metaData)) {
      closeAlert(session, "fileError")
      datatable(values$metaData, rownames = TRUE, options = list(
        columnDefs = list(list(targets = which(colnames(values$metaData) == "evalSame"), visible = FALSE))
      )) %>% formatStyle(
        ncol(values$metaData),
        valueColumns = which(colnames(values$metaData) == "evalSame"),
        target = "row",
        backgroundColor = styleEqual(1, "yellow")
      )
    }
    else {
      datatable(data.frame("Please enter a GSE ID"), rownames = FALSE, colnames = "NO DATA")
    }
  })

# summary -----------------------------------------------------------------

  
  #output$metaSummary <- renderText({printVarsSummary(values$metaData)})
  
  # Create the list of plot names
  plotInput <- reactive({
    if (!is.null(values$metaData)) {
      n_plot <- ncol(values$metaData)
      total_data <- lapply(1:n_plot, function(i){c(table(values$metaData[,i]), Num_NA=sum(is.na(values$metaData[,i])))})
      return (list("n_plot"=n_plot, "total_data"=total_data))
    }
  })
  
  # Create divs
  output$plots <- renderUI({
    
    if (!is.null(values$metaData)) {
      plot_output_list <- lapply(1:plotInput()$n_plot, function(i) {
        if (!grepl("evalSame", colnames(values$metaData)[i])) {
          plotname <- colnames(values$metaData)[i]
          plotHeight <- if(isAllNum(values$metaData[i])) 500 else 280
          plotOutput(plotname, height = plotHeight, width = "auto")
        }
      })   
      do.call(tagList, plot_output_list)
    }
  })
  # Create the actual plots associated with the plot names
  observe({
    if (!is.null(values$metaData)) {
      lapply(1:plotInput()$n_plot, function(i){
        output[[ colnames(values$metaData)[i] ]] <- renderPlot({
          #create a histogram if it's numeric, a barplot if it's a factor
          if (isAllNum(values$metaData[i])) {
            hist(as.numeric(as.character(plotInput()$total_data[[i]])), main = colnames(values$metaData)[i], xlab = "Value range", ylab = "Frequency", col = "darkblue", labels = TRUE)
          }
          else {
            if (isAllUnique(values$metaData[i])) {
              barplot(plotInput()$total_data[[i]], main = colnames(values$metaData)[i], xlab = "Values represented in the column", ylab = "Frequency", col = "cornflowerblue", legend.text = "All values are unique.")
              
              }
            else {
              barplot(plotInput()$total_data[[i]], main = colnames(values$metaData)[i], xlab = "Values represented in the column", ylab = "Frequency", col = "cornflowerblue", legend.text = TRUE)
              #print(plotInput()$total_data[[i]])
              }
          }
        })
      })
    }
  })
  

# merged warning ----------------------------------------------------------
  
  output$mergedWarning <- renderUI({
    #if any of the values$metaData last column are false, then color yellow and print the warning message
    #else, just put a grey box there or nothing at all
    if (!is.null(values$metaData) && any(values$metaData[,"evalSame"] == 1)) {
      wellPanel(id = "well",
            tags$head(tags$style(
              HTML('
                   #well {
                   background-color: #FFFF00;
                   }'))),
                   HTML("<p>There is more than one date in this dataset. This could indicate that this is two merged datasets.</p>"))
    }
  })

# extract columns ---------------------------------------------------------

  
  output$chooseSplitVars <- renderUI({
    colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    checkboxGroupInput(inputId = "colsToSplit", label = "Which columns contain key-value pairs?", colNames)
  })
  
  output$chooseDivideVars <- renderUI({
    colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    checkboxGroupInput(inputId = "colsToDivide", label = "Which columns contain multiple values?", colNames)
  })
  

  observeEvent(input$extractCols, ({values$lastData <- values$metaData
                                    values$metaData <- withProgress(extractCols(values$metaData, 
                                                              input$toSplit, 
                                                              input$colsToSplit, 
                                                              input$toDivide, 
                                                              input$colsToDivide, 
                                                              input$delimiter,
                                                              input$delimiter2,
                                                              input$allButSplit,
                                                              input$allButDivide), message = "Extracting columns")
                                    #WRITING COMMANDS TO R SCRIPT
                                    before <- length(values$oFile)
                                    values$oFile <- saveLines(commentify("extract values from columns with delimiter"), values$oFile)
                                    values$oFile <- saveLines(paste0("colsToSplit <- NULL"), values$oFile)
                                    for (i in 1:length(input$colsToSplit)) {
                                      values$oFile <- saveLines(paste0("colsToSplit[", i, "] <- ", formatString(input$colsToSplit[i])), values$oFile)
                                    }
                                    values$oFile <- saveLines(paste0("colsToDivide <- NULL"), values$oFile)
                                    for (i in 1:length(input$colsToDivide)) {
                                      values$oFile <- saveLines(paste0("colsToDivide[", i, "] <- ", formatString(input$colsToDivide[i])), values$oFile)
                                    }
                                    values$oFile <- saveLines(c(paste0("toSplit <- ", formatString(input$toSplit)), paste0("toDivide <- ", formatString(input$toDivide)),
                                                                paste0("delimiter <- ", formatString(input$delimiter)), 
                                                                paste0("delimiter2 <- ", formatString(input$delimiter2)),
                                                                paste0("allButSplit <- ", formatString(input$allButSplit)), 
                                                                paste0("allButDivide <- ", formatString(input$allButDivide)),
                                                                "metaData <- extractCols(metaData, toSplit, colsToSplit, toDivide, colsToDivide, delimiter, delimiter2, allButSplit, allButDivide)"), 
                                                              values$oFile)
                                    values$currChunkLen <- length(values$oFile) - before
  }))
 

# filter columns ----------------------------------------------------------

  
  output$chooseVarsToKeep <- renderUI({
    colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    checkboxGroupInput(inputId = "varsToKeep", label = "Which columns would you like to keep?", colNames)
  })
  
  observeEvent(input$filterCols, ({ if (!is.null(values$metaData)) {
                                      values$lastData <- values$metaData
                                      values$metaData <- filterCols(values$metaData, input$varsToKeep, input$allButKeep)
                                      
                                      #WRITING COMMANDS TO R SCRIPT
                                      before <- length(values$oFile)
                                      values$oFile <- saveLines(commentify("exclude undesired columns"), values$oFile)
                                      values$oFile <- saveLines(paste0("varsToKeep <- NULL"), values$oFile)
                                      for (i in 1:length(input$varsToKeep)) {
                                        values$oFile <- saveLines(paste0("varsToKeep[", i, "] <- ", formatString(input$varsToKeep[i])), values$oFile)
                                      }
                                      values$oFile <- saveLines(c(paste0("allButKeep <- ", formatString(input$allButKeep)),
                                                                  "metaData <- filterCols(metaData, varsToKeep, allButKeep)"), 
                                                                values$oFile)
                                      values$currChunkLen <- length(values$oFile) - before
                                      }
                                      }))
  #values$metaData <- eventReactive(input$toDivide, {})
  

# rename columns ----------------------------------------------------------

observe({
  input$colsToRename
  if (!is.null(input$colsToRename) && input$colsToRename != "" && (input$colsToRename %in% colnames(values$metaData))) {
    spacers <- c(" ", "\\.", "\\_")
    toSearch <- input$colsToRename
    #print(toSearch)
    for (x in spacers) {
      toSearch <- str_split(toSearch, x)[[1]]
      sep <- if (!all(nchar(toSearch) > 1)) " " else "|"
      #print(toSearch)
      toSearch <- paste(toSearch, collapse = sep)
    }
    synonyms <- unique(thesaurus.synonyms.feather$Code[which(grepl(toSearch, thesaurus.synonyms.feather$Synonyms, ignore.case = T))])
    values$thes_suggest <- c(thesaurus.preferred.feather$Preferred_Name[which(thesaurus.preferred.feather$Code %in% synonyms)], NA)
    #print(values$thes_suggest)
    #find newName %in% synonyms
                          # look up the matching indices in preferred names
  }
  #print(values$suggestions)
})
  
  #observeEvent(input$showCols, output$cols <- renderUI({
  #  for (col in output$colnames) {
  #    textOutput(col)
  #  }
  #}))
  output$showCols <- renderUI({
    colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    setNames(colNames, colNames)
    selectInput(inputId = "colsToRename", label = "Which columns would you like to rename?", choices = colNames)
  })
  
  output$newName <- renderRHandsontable({
    rhandsontable(data.frame(`Enter a new name: ` = "", stringsAsFactors = FALSE, check.names = F), width = 250, height = 300, stretchH = "all", rowHeaders = FALSE) %>% 
      hot_col(col = "Enter a new name: ", type = "autocomplete", source = values$thes_suggest, strict = FALSE)
  })
  
  observe({
    if (!is.null(input$newName)) {
      values$newName <- hot_to_r(input$newName)[which(colnames(hot_to_r(input$newName)) == "Enter a new name: "),]
      #print(values$DFIn)
    }
  })
  
  output$thesLink <- renderUI({
    if (!identical(as.character(thesaurus.preferred.feather[which(thesaurus.preferred.feather$Preferred_Name == values$newName), "Code"]), "character(0)")) {
      url <- a(target = "_blank", href = paste0("https://ncit.nci.nih.gov/ncitbrowser/ConceptReport.jsp?dictionary=NCI_Thesaurus&ns=ncit&code=", 
                                                as.character(thesaurus.preferred.feather[which(thesaurus.preferred.feather$Preferred_Name == values$newName), "Code"])), 
               values$newName)
      tagList("NCI Thesaurus link:", url)
    }
  })
  
  #newNameCount <- reactive(input$addPrefName)
  
  #observeEvent(input$addPrefName, {
  #  cat(newNameCount())
  #  insertUI("#colsToRename", where = "beforeEnd", ui = textInput(paste("newName",newNameCount(), sep = ""), "Enter a new name: "))
  #  values$newNames <- c(values$newNames, input[[paste("newName", newNameCount() - 1, sep = "")]])
  #})
  
  observeEvent(input$save, {
    values$newNames[[input$colsToRename]] <- if (!is.null(values$newName)) values$newName else input$colsToRename
  })
  
  observeEvent(input$delete, {
    values$newNames <- values$newNames[-(length(values$newNames))]
  })
  
  output$new_name_contents <- renderTable(colnames = FALSE, {
    data.frame(names(values$newNames), values$newNames)
  })
  
  observeEvent(input$rename, ({ 
    #print(values$newNames)
    #values$newNames <- c(values$newNames, input[[paste("newName", newNameCount(), sep = "")]])
    values$lastData <- values$metaData
    values$metaData <- renameCols(values$metaData, values$newNames, session)
    
    #WRITING COMMANDS TO R SCRIPT
    before <- length(values$oFile)
    values$oFile <- saveLines(commentify("rename columns"), values$oFile)
    values$oFile <- saveLines(paste0("newNames <- NULL"), values$oFile)
    for (i in 1:length(values$newNames)) {
      values$oFile <- saveLines(paste0("newNames[[", formatString(names(values$newNames)[i]), "]] <- ", formatString(values$newNames[[i]])), values$oFile)
    }
    values$oFile <- saveLines("metaData <- renameCols(metaData, newNames)", 
                              values$oFile)
    values$currChunkLen <- length(values$oFile) - before
    
    values$newNames <- NULL
    #print("reachedEnd")
    }))
  
  
  #observeEvent(input$removeBtn, {
  #  removeUI(
      ## pass in appropriate div id
  #    selector = paste0('#', paste("newName",length(values$newNames)+1, sep = ""))
  #  )
  #  values$newNames <<- values$newNames[-length(values$newNames)]
  #})


# substitute vals ---------------------------------------------------------

  output$sliderSub <- renderUI({
    
    if(isAllNum(values$metaData[which(colnames(values$metaData) == input$colsToSub)])) {
      output = tagList()
      currCol <- as.numeric(as.character(currCol <- values$metaData[!is.na(values$metaData[,input$colsToSub]),input$colsToSub]))
      output[[1]] <- sliderInput(inputId = "slideInSub", label = "", min = min(currCol), max = max(currCol), value = c(quantile(currCol)[2], quantile(currCol)[3]))
      output[[2]] <- textInput("newRangeVal", label = "Please enter a value to replace all values in the range:")
      output
    }
    else {
      p(style = "color:red", "Looks like this column isn't numeric!")
    }
    
  })
  
  output$showColsForSub <- renderUI({
    colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    setNames(colNames, colNames)
    selectInput(inputId = "colsToSub", label = div("Please select a column with values to substitute: ", 
                                                   helpButton("Values to substitute may include values to treat as missing, such as ? or --")), 
                choices = colNames)
  })
  
  observeEvent(input$colsToSub, {
    
    values$suggestions <- if (!is.null(input$colsToSub) && input$colsToSub != "" && (input$colsToSub %in% colnames(values$metaData))) unique(as.character(values$metaData[,input$colsToSub]))
    #print(values$suggestions)
  }, ignoreInit = T, ignoreNULL = T)
  
  if(F) {
  observe({
    values$DFIn
    if ("To_Replace" %in% colnames(values$DFIn)) {
      val <- values$DFIn$To_Replace
      #for (val in vals) {
      #print(val)
      if (!is.null(val) && val != "") {
        spacers <- c(" ", "\\.", "\\_")
        toSearch <- val
        for (x in spacers) {
          toSearch <- str_split(toSearch, x)[[1]]
          sep <- if (!all(nchar(toSearch) > 1)) " " else "|"
          toSearch <- paste(toSearch, collapse = sep)
        }
        #print(toSearch)
        synonyms <- unique(thesaurus.synonyms.feather$Code[which(grepl(toSearch, thesaurus.synonyms.feather$Synonyms, ignore.case = T))])
        values$thes_suggest_vals <- c(thesaurus.preferred.feather$Preferred_Name[which(thesaurus.preferred.feather$Code %in% synonyms)], NA)
        #print(values$thes_suggest_vals)
      }
      #}
    }
  })
  }
  
  observeEvent(input$hotIn, {
    
    
    val <- if(!is.null(input$hotIn)) hot_to_r(input$hotIn)$To_Replace else ""
    
    if (!is.null(val) && val != "" && !identical(val, character(0))) {
      
      synonyms <- unique(thesaurus.synonyms.feather$Code[which(grepl(val, thesaurus.synonyms.feather$Synonyms, ignore.case = T))])
      
      if(length(synonyms > 0)) {
        values$thes_suggest_vals <- c(thesaurus.preferred.feather$Preferred_Name[which(thesaurus.preferred.feather$Code %in% synonyms)], NA)
      }
      else {
        spacers <- c(" ", "\\.", "\\_")
        toSearch <- val
        for (x in spacers) {
          toSearch <- str_split(toSearch, x)[[1]]
          sep <- if (!all(nchar(toSearch) > 1)) " " else "|"
          toSearch <- paste(toSearch, collapse = sep)
        }
        #print(toSearch)
        synonyms <- unique(thesaurus.synonyms.feather$Code[which(grepl(toSearch, thesaurus.synonyms.feather$Synonyms, ignore.case = T))])
        values$thes_suggest_vals <- c(thesaurus.preferred.feather$Preferred_Name[which(thesaurus.preferred.feather$Code %in% synonyms)], NA)
      }
      
      #print(values$thes_suggest_vals)
    }
      #}
  }, ignoreInit = T, ignoreNULL = T)
  
  output$thesLinkSub <- renderUI({
    if (!identical(as.character(thesaurus.preferred.feather[which(thesaurus.preferred.feather$Preferred_Name == values$DFIn[,"New_Val"]), "Code"]), "character(0)")) {
    url <- a(target = "_blank", href = paste0("https://ncit.nci.nih.gov/ncitbrowser/ConceptReport.jsp?dictionary=NCI_Thesaurus&ns=ncit&code=", 
                           as.character(thesaurus.preferred.feather[which(thesaurus.preferred.feather$Preferred_Name == values$DFIn[,"New_Val"]), "Code"])), 
             values$DFIn[,"New_Val"])
    tagList("NCI Thesaurus link:", url)
    }
  })
  
  output$hotIn <- renderRHandsontable({
    rhandsontable(data.frame(To_Replace = "", New_Val = "", stringsAsFactors = FALSE), width = 250, height = 300, stretchH = "all", rowHeaders = FALSE) %>% 
      hot_col(col = "To_Replace", type = "autocomplete", source = values$suggestions, strict = FALSE) %>%
      hot_col(col = "New_Val", type = "autocomplete", source = values$thes_suggest_vals, strict = FALSE)
  })
  
  output$hotOut <- renderDT({
    datatable(if(!is.null(values$tablesList[[input$colsToSub]])) values$tablesList[[input$colsToSub]] else values$DFOut, options = list(paging = F, searching = F))
  })
  
  observeEvent(input$hotIn, {
    #if (!is.null(input$hotIn)) {
      values$DFIn <- hot_to_r(input$hotIn)
      #print(values$DFIn)
    #}
  }, ignoreInit = T, ignoreNULL = T)

  eventReactive(input$colsToSub, {
    currentCol <- input$colsToSub
    #print(currentCol)
    #print(values$tablesList[[currentCol]])
    if(!("To_Replace" %in% colnames(values$tablesList[[currentCol]]))) {
      #cat("true")
      values$tablesList[[currentCol]] <- data.frame(To_Replace = "", stringsAsFactors = FALSE)
      #print(values$tablesList[currentCol])
      values$tablesList[[currentCol]] <- cbind(values$tablesList[[currentCol]], New_Val = "")
    }
    values$DFOut <- values$tablesList[[currentCol]]
  }) 
  
  observeEvent(input$addToSub, {
    #print(input$colsToSub)
    if (!is.null(input$colsToSub) && input$colsToSub != "") {
    #format the table correctly according to the colsToSub selected
    currentCol <- input$colsToSub
    currentDF <- values$tablesList[[currentCol]]
    #print(currentCol)
    #print(values$tablesList[[currentCol]])
    #if(input$isRange) {
      #add range to To_Replace and text input to New_Val
    #}
    if(all(currentDF["To_Replace"] == "")) {
      #cat("true")
      values$tablesList[[currentCol]] <- data.frame(
        To_Replace = if(input$isRange) paste("RANGE:", paste(input$slideInSub, collapse = "-")) else values$DFIn["To_Replace"], 
        stringsAsFactors = FALSE)
      #print(values$tablesList[currentCol])
      values$tablesList[[currentCol]] <- cbind(values$tablesList[[currentCol]], 
                                               New_Val = if(input$isRange) input$newRangeVal else values$DFIn["New_Val"], stringsAsFactors = F)
    }
    #print(values$DFIn[["To_Replace"]])
    #print(currentDF[["To_Replace"]])
    #if the values are not there, add them to the end of the table
    else if((values$DFIn[["To_Replace"]] %in% currentDF[["To_Replace"]]) || 
            (paste("RANGE:", paste(input$slideInSub, collapse = "-")) %in% currentDF[["To_Replace"]])) {
      if(input$isRange) {
        currentDF[which(paste("RANGE:", paste(input$slideInSub, collapse = "-")) %in% currentDF[["To_Replace"]]),] <- 
          c(paste("RANGE:", paste(input$slideInSub, collapse = "-")), input$newRangeVal)
      }
      else {
        currentDF[which(currentDF[["To_Replace"]] == values$DFIn[["To_Replace"]]),] <- values$DFIn[1,]
      }
      values$tablesList[[currentCol]] <- currentDF
    }
    else {
      #print(values$tablesList[[currentCol]])
      #print(values$DFIn[1])
      values$tablesList[[currentCol]] <- rbind(values$tablesList[[currentCol]], 
                                               if(input$isRange) c(paste("RANGE:", paste(input$slideInSub, collapse = "-")), 
                                                                   input$newRangeVal) else values$DFIn[1,])
    }
    }
  })
  
  observeEvent(input$removeToSub, {
    #print(input$hotOut_rows_selected)
    if (!is.null(input$colsToSub) && !is.null(values$tablesList[[input$colsToSub]])) {
    if (!identical(values$tablesList[[input$colsToSub]][,"To_Replace"], character(0)) && values$tablesList[[input$colsToSub]][,"To_Replace"] != "") {
      values$tablesList[[input$colsToSub]] <- values$tablesList[[input$colsToSub]][-input$hotOut_rows_selected,]
      values$DFOut <- values$tablesList[[input$colsToSub]]
    }
    if (identical(values$tablesList[[input$colsToSub]][,"To_Replace"], character(0))) {
      values$tablesList[[input$colsToSub]][1,] <- c("", "")
      values$DFOut <- values$tablesList[[input$colsToSub]]
    }
    }
  })
  
  observeEvent(input$evaluateSubs, {#print(values$tablesList)
    if (!identical(values$tablesList, list())) {
      values$lastData <- values$metaData
      values$metaData <- withProgress(substituteVals(values$metaData, values$tablesList))
  
      #WRITING COMMANDS TO R SCRIPT
      before <- length(values$oFile)
      values$oFile <- saveLines(commentify("substitute values"), values$oFile)
      values$oFile <- saveLines(paste0("tablesList <- NULL"), values$oFile)
      for (i in 1:length(values$tablesList)) {
        values$oFile <- saveLines(paste0("tablesList[[", formatString(names(values$tablesList)[i]), "]] <- ",
                                         "data.frame(", colnames(values$tablesList[[i]])[1], "=c(", 
                                         paste(formatString(as.character(values$tablesList[[i]][,1])), collapse = ", "), "), ",
                                         colnames(values$tablesList[[i]])[2], "=c(", 
                                         paste(formatString(as.character(values$tablesList[[i]][,2])), collapse = ", "), "))"), values$oFile)
      }
      values$oFile <- saveLines("metaData <- substituteVals(metaData, tablesList)", 
                                values$oFile)
      values$currChunkLen <- length(values$oFile) - before
      
      values$tablesList <- list()
      values$DFOut <- data.frame(To_Replace = "", New_Val = "", stringsAsFactors = FALSE)
    
    }
  })

# exclude vals ------------------------------------------------------------

  
  output$sliderExclude <- renderUI({
    
    if(isAllNum(values$metaData[input$col_valsToExclude])) {
      output <- tagList()
      currCol <- as.numeric(as.character(currCol <- values$metaData[!is.na(values$metaData[,input$col_valsToExclude]),input$col_valsToExclude]))
      output[[1]] <- radioButtons("excludeToKeep", label = "I would like to:", 
                                  choices = list("exclude the values within the range." = "exclude", 
                                                 "keep only the values within the range." = "keep"))
      output[[2]] <- sliderInput(inputId = "sliderExclude", label = paste0("Please choose a range of values:"), min = min(currCol), max = max(currCol), value = c(quantile(currCol)[2], quantile(currCol)[3]))
      output
    }
    else {
      p(style = "color:red", "Looks like this column isn't numeric!")
    }
    
  })
  
  output$showColsForExclude <- renderUI({
    colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    setNames(colNames, colNames)
    selectInput(inputId = "col_valsToExclude", label = div("Please select a column with values to exclude: ", 
                                                   helpButton("Values to exclude may include missing values, such as NA.")), 
                choices = colNames)
  })
  
  output$showValsForExclude <- renderUI({
    valNames <- unique(as.character(values$metaData[,input$col_valsToExclude]))
    valNames[which(is.na(valNames))] <- "NA"
    #print(valNames)
    checkboxGroupInput(inputId = "valsToExclude", 
                       label = div("Which variables would you like to exclude?", 
                                   helpButton("Excluding a variable will remove the entire row that contains that variable.")),
                                   choices = valNames)
  })
  
  observe({
    input$valsToExclude
    currentCol <- input$col_valsToExclude
    if (!is.null(currentCol)) {
      if(input$isRangeExclude) {
        values$excludesList[[currentCol]] <-  paste(input$excludeToKeep, paste(input$sliderExclude, collapse = "-"), sep = ": ")
      } 
      else if (!is.null(input$valsToExclude)) {
        values$excludesList[[currentCol]] <- input$valsToExclude
      }
      else if (currentCol %in% names(values$excludesList)) {
        values$excludesList <- values$excludesList[-which(names(values$excludesList) == currentCol)]
      }
    }
  })
  
  observeEvent(input$excludeVals, {
    
    #print(values$excludesList)
    values$lastData <- values$metaData
    values$metaData <- withProgress(excludeVars(values$metaData, values$excludesList))
    
    #WRITING COMMANDS TO R SCRIPT
    before <- length(values$oFile)
    values$oFile <- saveLines(commentify("exclude undesired samples"), values$oFile)
    values$oFile <- saveLines(paste0("excludesList <- NULL"), values$oFile)
    for (i in 1:length(values$excludesList)) {
      values$oFile <- saveLines(paste0("excludesList[[", formatString(names(values$excludesList)[i]), "]] <- ", 
                                       formatString(values$excludesList[[i]])), values$oFile)
    }
    values$oFile <- saveLines("metaData <- excludeVars(metaData, excludesList)", 
                              values$oFile)
    values$currChunkLen <- length(values$oFile) - before
    
    values$excludesList <- list()
  })
  


# download data -----------------------------------------------------------
  
  output$nameFile <- renderUI({
    textInput("userFileName", label = NULL, value = paste0("file name.", input$fileType))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      #paste(input$geoID, "_Clinical.", input$fileType, sep = "")
      input$userFileName
    },
    content = function(file) {
      myData <- values$metaData
      myData <- myData[-which(grepl("evalSame", colnames(myData)))]
      myData <- cbind(rownames(myData), myData)
      colnames(myData)[1] <- ""
      
      #WRITING COMMANDS TO R SCRIPT
      before <- length(values$oFile)
      values$oFile <- saveLines(commentify("save data"), values$oFile)
      values$oFile <- saveLines(c("metaData <- cbind(rownames(metaData), metaData)", "colnames(metaData)[1] <- ''", 
                                  paste0("file <- ", formatString(input$userFileName))), values$oFile)
      
      if (input$fileType == "csv") {
        write.csv(myData, file, row.names = FALSE, col.names = TRUE)
        
        #WRITING COMMANDS TO R SCRIPT
        values$oFile <- saveLines(paste0("write.csv(metaData, file, row.names = FALSE, col.names = TRUE)"), values$oFile)
        
      }
      else if (input$fileType == "tsv") {
        write.table(myData, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
        
        #WRITING COMMANDS TO R SCRIPT
        values$oFile <- saveLines("write.table(metaData, file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)", 
                                  values$oFile)
        
      }
      else if (input$fileType == "JSON") {
        library(jsonlite)
        library(readr)
        
        myData %>% toJSON() %>% write_lines(file)
        
        #WRITING COMMANDS TO R SCRIPT
        values$oFile <- saveLines(c("library(jsonlite)", "library(readr)", 
                                    "metaData %>% toJSON() %>% write_lines(file)"), 
                                  values$oFile)
      }
      
      values$currChunkLen <- length(values$oFile) - before

    }#,
    #contentType = "text/tsv"
  )
  
  output$downloadRscript <- downloadHandler(
    filename = function() {
      paste0(input$geoID, "_Clinical_Rscript.R")
    },
    content = function(file) {
      saveToRscript(values$oFile, file)
    }
  )
  
  observeEvent(input$saveMetadata, {
    if (!is.null(values$metaData)) {
      
      fileInfo <- parseSavePath(values$volumes, input$saveMetadata)
      
      file <- fileInfo$datapath
      
      myData <- values$metaData
      myData <- myData[-which(grepl("evalSame", colnames(myData)))]
      myData <- cbind(rownames(myData), myData)
      colnames(myData)[1] <- ""
      
      #WRITING COMMANDS TO R SCRIPT
      before <- length(values$oFile)
      values$oFile <- saveLines(commentify("save data"), values$oFile)
      values$oFile <- saveLines(c("metaData <- cbind(rownames(metaData), metaData)", "colnames(metaData)[1] <- ''", 
                                  paste0("file <- ", formatString(input$userFileName))), values$oFile)
      
      if (input$fileType == "csv") {
        write.csv(myData, file, row.names = FALSE, col.names = TRUE)
        
        #WRITING COMMANDS TO R SCRIPT
        values$oFile <- saveLines(paste0("write.csv(metaData, file, row.names = FALSE, col.names = TRUE)"), values$oFile)
        
      }
      else if (input$fileType == "tsv") {
        write.table(myData, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
        
        #WRITING COMMANDS TO R SCRIPT
        values$oFile <- saveLines("write.table(metaData, file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)", 
                                  values$oFile)
        
      }
      else if (input$fileType == "JSON") {
        library(jsonlite)
        library(readr)
        
        myData %>% toJSON() %>% write_lines(file)
        
        #WRITING COMMANDS TO R SCRIPT
        values$oFile <- saveLines(c("library(jsonlite)", "library(readr)", 
                                    "metaData %>% toJSON() %>% write_lines(file)"), 
                                  values$oFile)
      }
      
      values$currChunkLen <- length(values$oFile) - before
    } else {
      showNotification("Please load some data.")
    }
  })
  
  observeEvent(input$showHistory, {
    saveToRscript(values$oFile)
    saveRscript()
  })

# navigation --------------------------------------------------------------

#1  
  output$nav_1_ui <- renderUI({
    fluidRow(
      #actionButton('nav_4_to_3_button', 'Back'),
      column(2, offset = 8, actionButton('nav_1_to_2_button', 'Next'))
    )
    })
  observeEvent(input$nav_1_to_2_button, {
    updateTabsetPanel(session, 'sidePanel', selected = '2')
    })
#2  
  output$nav_2_ui <- renderUI({
    fluidRow(
      column(1, actionButton('nav_2_to_1_button', 'Back')),
      column(2, offset = 7, actionButton('nav_2_to_3_button', 'Next'))
      )
  })
  observeEvent(input$nav_2_to_1_button, {
    updateTabsetPanel(session, 'sidePanel', selected = '1')
  })
  observeEvent(input$nav_2_to_3_button, {
    updateTabsetPanel(session, 'sidePanel', selected = '3')
  }) 
#3  
  output$nav_3_ui <- renderUI({
    fluidRow(
      column(1, actionButton('nav_3_to_2_button', 'Back')),
      column(2, offset = 7, actionButton('nav_3_to_4_button', 'Next'))
    )
  })
  observeEvent(input$nav_3_to_2_button, {
    updateTabsetPanel(session, 'sidePanel', selected = '2')
  })
  observeEvent(input$nav_3_to_4_button, {
    updateTabsetPanel(session, 'sidePanel', selected = '4')
  })
#4  
  output$nav_4_ui <- renderUI({
    fluidRow(
      column(1, actionButton('nav_4_to_3_button', 'Back')),
      column(2, offset = 7, actionButton('nav_4_to_5_button', 'Next'))
    )
  })
  observeEvent(input$nav_4_to_3_button, {
    updateTabsetPanel(session, 'sidePanel', selected = '3')
  })
  observeEvent(input$nav_4_to_5_button, {
    closeAlert(session, "offendingChars")
    updateTabsetPanel(session, 'sidePanel', selected = '5')
  })
#5
  output$nav_5_ui <- renderUI({
    fluidRow(
      column(1, actionButton('nav_5_to_4_button', 'Back')),
      column(2, offset = 7, actionButton('nav_5_to_6_button', 'Next'))
    )
  })
  observeEvent(input$nav_5_to_4_button, {
    updateTabsetPanel(session, 'sidePanel', selected = '4')
  })
  observeEvent(input$nav_5_to_6_button, {
    updateTabsetPanel(session, 'sidePanel', selected = '6')
  })
#6
  output$nav_6_ui <- renderUI({
    fluidRow(
      column(1, actionButton('nav_6_to_5_button', 'Back'))
      #column(2, offset = 7, actionButton('nav_6_to_7_button', 'Next'))
    )
  })
  observeEvent(input$nav_6_to_5_button, {
    updateTabsetPanel(session, 'sidePanel', selected = '5')
  })
  #observeEvent(input$nav_6_to_7_button, {
  #  updateTabsetPanel(session, 'sidePanel', selected = '7')
  #})
  

# expression data ---------------------------------------------------------

  output$expressionData <- DT::renderDT({
    if(!is.null(values$exprToDisplay)) {
      datatable(values$exprToDisplay, rownames = FALSE, options = list(dom = "ft"))
    }
    else {
      datatable(data.frame("Please download some expression data"), rownames = FALSE, 
                           colnames = "NO DATA", options = list(dom = "ft"))
    }
  })
  
  observeEvent(input$transposeExpr, {
    #will cause R to shut down
    #values$exprData <- t(values$exprData)
    #values$exprData <- values$exprData %>%
    #  gather(newrows, valname, -probes) %>%
    #  spread(probes, valname)
  })
  
  output$exprLabels <- renderUI({
    selectInput("colForExprLabels", label = "Please select a column to replace the probe set IDs", 
                choices = colnames(values$ftToDisplay))
  })
  
  output$summarizeOptions <- renderUI({
    if(!input$colForExprLabels %in% findExprLabelColumns(values$ftToDisplay)) {
      selectInput("howToSummarize", label = "It looks like this column contains multiple values for one expression ID.
                  How would you like to summarize the data?", choices = c("mean", "median", "max", "min", "keep all"))
    }
  })
  
  observeEvent(input$previewExpr, {
    
    values$lastExprToDisplay <- values$exprToDisplay
    
    if(!input$useExistingExprLabels) {
      values$exprToDisplay <- replaceRowNames(values$exprToDisplay, values$ftToDisplay, input$colForExprLabels)
    }
    
    if(input$transposeExpr) {
      values$exprToDisplay <- quickTranspose(values$exprToDisplay)
    }
  })
  
  observeEvent(input$undoEvalExpr, {
    values$exprToDisplay <- values$lastExprToDisplay
  })
  

# feature data ------------------------------------------------------------

  output$featureData <- DT::renderDT({
    if(!is.null(values$ftToDisplay)) {
      datatable(values$ftToDisplay, rownames = FALSE, options = list(dom = "ft", columnDefs = list(list(
        targets = "_all",
        #Makes it so that the table will only display the first 30 chars.
        #See https://rstudio.github.io/DT/options.html
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && typeof data === 'string' && data.length > 30 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
          "}")
      ))))
    }
    else {
      datatable(data.frame("Please download some feature data"), rownames = FALSE, 
                colnames = "NO DATA", options = list(dom = "ft"))
    }
  })  
}

shinyApp(ui = ui, server = server)
