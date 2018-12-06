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
library(tidyverse)
library(shinyWidgets)
source("geocurateFunctions.R")

series_list <- read_feather("www/series_list.feather")
platform_list <- read_feather("www/platform_list.feather")

# help icon to add as tag to buttons, etc ---------------------------------


help_button <- function(message = "content", placement = "right") {
  tipify(icon("question-circle"), title = message, placement = placement, trigger = "hover")
}

help_link <- function(id) {
  actionLink(inputId = id, label = icon("question-circle"))
  #clickable icon that can be added to any text
  #upon click, the icon shows a modal specific to that icon
  #the modal contains any instructions and images associated with that icon
  #showModal(modalDialog(includeMarkdown(help_file), 
  #                      footer = modalButton()))
}

help_modal <- function(help_file, images_id) {
  showModal(
    modalDialog(
      includeMarkdown(help_file),
      uiOutput(images_id),
      tags$script(HTML(
        "$(document).on('click', '.clickimg', function() {",
        "  Shiny.onInputChange('clickimg', $(this).data('value'));",
        "});"
      )),
      footer = modalButton("Close"),
      size = "l"
      )
    )
}


# creating an image grid for help modals ----------------------------------


create_image_grid <- function(images, image_names) {
  fluidRow(
    mapply(function(my_image, img_name) {
      column(3, 
             div(tags$img(src = my_image, width = "200px", class = "clickimg", "data-value" = my_image), img_name)
      )
    }, images, image_names, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  )
}

# colored buttons of different types --------------------------------------


primary_button <- function(id, label, icon = NULL) {
  shiny::actionButton(id, div(label, icon), 
               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
}

secondary_button <- function(id, label, icon = NULL) {
  shiny::actionButton(id, div(label, icon), 
               style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f")
}

tertiary_button <- function(id, label, icon = NULL) {
  shiny::actionButton(id, div(label, icon), 
               style = "color: #fff; background-color: #6baed6; border-color: #6baed6")
}

# detects variable type & formats string to be written to R script --------


format_string <- function(element) {
  suppressWarnings(if (is.null(element)) {
    return("NULL")
  }
  else if (is.na(element)) {
    return("NA")
  }
  else if (mode(element) == "numeric" ||
           mode(element) == "logical") {
    element <- as.character(element)
  }
  else if (mode(element) == "character") {
    element <-
      sapply(element, function(x) {
        paste0("'", x, "'")
      }, USE.NAMES = FALSE)
  })
  if (length(element) > 1) {
    element <- paste0("c(", paste(element, collapse = ", "), ")")
  }
  return(element)
}

# creates section headings for R script -----------------------------------


commentify <- function(message) {
  
  num_chars <- 75
  comment <- paste0("# ", message, " ")
  comment <- paste0(comment, paste(rep("-", num_chars - nchar(comment)), collapse = ""))
  c("", "", comment, "")
}

options(shiny.autoreload = F)

# UI ----------------------------------------------------------------------


ui <- fluidPage(
  #centers loading bars in the middle of the page
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           height: 50px;
           width: 800px;
           position:fixed;
           top: calc(50% - 50px);;
           left: calc(50% - 400px);;
           }
           ")
      ),
    #For finding which plot the user wants to save
    #https://stackoverflow.com/questions/40168801/r-shiny-last-clicked-button-id
    tags$script(HTML("$(document).on('click', '.clinical_plot', function () {
                                Shiny.onInputChange('last_btn_clinical',this.id);
                             });"),
                HTML("$(document).on('click', '.expression_plot', function () {
                                Shiny.onInputChange('last_btn_expression',this.id);
                             });"))
    ),
  navbarPage(title = "GEOcurate", id = "top_level",
             tabPanel(title = "Clinical data",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          useShinyjs(),
                          tabsetPanel(id = "clinical_side_panel",
                                      
                                      
                                      # download data -----------------------------------------------------------
                                      
                                      
                                      tabPanel("1",
                                               h4("Importing the data"),
                                               div("Welcome to GEOcurate! This application will allow you to reformat data
                                                   from ",
                                                 a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/", "Gene Expression Omnibus"),
                                                 ", which can then be used to answer research questions. To get started,",
                                                 a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/gds", "find a series of interest"),
                                                   "and take a look at the help documentation or"
                                                 ),
                                               selectizeInput(inputId = "geoID", label = div("Please input a GSE ID: ", 
                                                                                        help_link(id = "download_help")), choices = NULL),
                                               uiOutput("gse_link"),
                                               br(),
                                               checkboxGroupInput(inputId = "download_data_filter", label = div("Remove columns in which every value...", 
                                                                                                  help_button("Removes columns right after downloading, according to the following specifications.")),
                                                                  choiceNames = list("Is the same", 
                                                                                     "Is unique",
                                                                                     "Contains a date", 
                                                                                     "Is a web address"),
                                                                  choiceValues = list("same_vals", "all_diff", "dates", "url")),
                                               primary_button(id = "download_data_evaluate", label = "Import"),
                                               hr(), uiOutput("nav_1_ui")
                                      ),
                                      
                                      
                                      # split variables ---------------------------------------------------------
                                      
                                      
                                      #specify which columns to split apart, and the delimiter
                                      tabPanel("2",
                                               h4("Formatting the data"),
                                               p("Sometimes columns contain multiple values in them. This makes it so that the values
                                                 cannot be analyzed separately. Here, you can indicate that there are multiple values in a
                                                 column so that the values can be separate."),
                                               checkboxInput(inputId = "to_split", label = div("Choose columns with key-value pairs separated by a delimiter",
                                                                                               help_link(id = "split_help")
                                                                                               )),
                                               #div(actionLink(inputId = "to_split", label = icon("caret-square-o-down")), "Contains key-value pairs separated by a delimiter", help_link(id = "split_help")),                                                
                                               #uiOutput("to_split_toggle"),
                                               #prettyToggle(inputId = "to_split", 
                                              #              label_on = div("Columns contain key-value pairs",
                                              #                             help_link(id = "split_help")),
                                              #              label_off = div("Columns contain key-value pairs",
                                              #                              help_link(id = "split_help")),
                                              #              icon_on = icon("caret-up"),
                                              #              icon_off = icon("caret-down"),
                                              #              status_on = "info",
                                              #              status_off = "primary",
                                              #              shape = "curve",
                                              #              animation = "rotate"),
                                               conditionalPanel(condition = "input.to_split == true",
                                                                uiOutput("choose_cols_to_split"),
                                                                checkboxInput(inputId = "split_all_but", label = tags$i("Split all BUT the specified")),
                                                                textInput(inputId = "split_delimiter", label = "Delimiter (including any spaces): ")
                                               ),
                                               checkboxInput(inputId = "to_divide", label = div("Choose columns with multiple values in one column",
                                                                                                help_link(id = "divide_help"))),
                                              #prettyToggle(inputId = "to_divide", 
                                              #             label_on = div("Columns contain multiple values",
                                              #                            help_link(id = "divide_help")),
                                              #             label_off = div("Columns contain multiple values",
                                              #                             help_link(id = "divide_help")),
                                              #             icon_on = icon("caret-up"),
                                              #             icon_off = icon("caret-down"),
                                              #v            status_on = "info",
                                              #             status_off = "primary",
                                              #             shape = "curve",
                                              #             animation = "rotate"),
                                               conditionalPanel(condition = "input.to_divide == true",
                                                                uiOutput("choose_cols_to_divide"),
                                                                checkboxInput(inputId = "divide_all_but", label = tags$i("Split all BUT the specified")),
                                                                textInput(inputId = "divide_delimiter", label = "Delimiter (including any spaces): ")
                                               ),
                                               primary_button(id = "reformat_columns", label = "Reformat columns"),
                                               hr(), uiOutput("nav_2_ui")
                                      ),
                                      
                                      # exclude columns ---------------------------------------------------------
                                      
                                      
                                      #specify which vars to keep
                                      tabPanel("3",
                                               h4("Selecting informative columns"),
                                               p("It can be helpful to filter out unneeded columns for better storage capacity and improved
                                                 human readability. Here, you can choose which columns are most important for you to keep
                                                 and drop the rest."),
                                               uiOutput("display_vars_to_keep"),
                                               checkboxInput(inputId = "keep_all_but", label = tags$i("Keep all BUT the specified")),
                                               primary_button(id = "clinical_evaluate_filters", label = "Filter columns"),
                                               hr(), uiOutput("nav_3_ui")
                                      ),
                                      
                                      # rename columns ----------------------------------------------------------
                                      
                                      
                                      #renaming any columns
                                      tabPanel("4",
                                               h4("Renaming columns"),
                                               p("In order to integrate the data with other tables or for humans to be able to understand the data,
                                                 it may be helpful to replace the existing column names with more accurate/descriptive ones.
                                                 Here, you can give any column a new name."),
                                               uiOutput("display_cols_to_rename"),
                                               #rHandsontableOutput("newName"),
                                               textInput(inputId = "rename_new_name", label = "Please specify a new name for the column."),
                                               primary_button(id = "rename", label = "Rename columns"),
                                               hr(), uiOutput("nav_4_ui")
                                      ),
                                      
                                      # substitute --------------------------------------------------------------
                                      
                                      
                                      #specify which values to substitute for other values/which values should be treated as NA
                                      tabPanel("5",
                                               h4("Substituting values"),
                                               p("In order to achieve the uniformity required to combine datasets, it may be helpful to substitute 
                                                 some of the values in the data for other values. Here, you can identify values you would like to replace
                                                 and an alternative to replace them with."),
                                               uiOutput("display_cols_to_sub"),
                                               checkboxInput(inputId = "substitute_isrange", label = "Specify a range of values to substitute?"),
                                               conditionalPanel(condition = "input.substitute_isrange == true",
                                                                uiOutput("input_sub_range")),
                                               #conditionalPanel(condition = "input.substitute_isrange == false",
                                                                h5('Click "Add" to add rows or "Remove" to remove the last row.'),
                                                                rHandsontableOutput("input_subs_table")#)
                                               ,
                                               #div(#style = 'overflow-x: scroll', 
                                              #   DTOutput("display_user_subs")),
                                               primary_button("evaluate_subs", "Substitute"),
                                               hr(), uiOutput("nav_5_ui")
                                      ),
                                      
                                      # exclude variables -------------------------------------------------------
                                      
                                      
                                      tabPanel("6",
                                               h4("Filtering samples"),
                                               p("You may want to remove some of the values in a column, for example, if you have missing (NA) values.
                                                  Here, you can specify which values you would like to remove.
                                                 Excluding a value will take out the entire row that contains that value in the selected column."),
                                               uiOutput("display_cols_for_exclude"),
                                               checkboxInput("exclude_isrange", "Specify a range of values"),
                                               conditionalPanel(condition = "input.exclude_isrange == true",
                                                                uiOutput("sliderExclude")),
                                               conditionalPanel(condition = "input.exclude_isrange == false",
                                                                uiOutput("display_vals_to_exclude")),
                                               primary_button("clinical_evaluate_exclude", "Exclude"),
                                               hr(), uiOutput("nav_6_ui")
                                      ),
                                      tabPanel("7",
                                               h4("Saving the data"),
                                               p("Here is where you can download the clinical data to your computer. If you have R installed,
                                                 you can also download the R script that produced this data. The R script allows other scientists
                                                to replicate your experiment because it shows how the data was obtained."),
                                               radioButtons("clinical_file_type", "File type:", 
                                                            choices = c("Comma-separated file" = "csv", "Tab-separated file" = "tsv", 
                                                                        "JSON" = "JSON", "Excel" = "xlsx")),
                                               uiOutput("clinical_display_filename"),
                                               tags$b("Download:"),
                                               fluidRow(
                                                 column(1, downloadButton("clinical_evaluate_save", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                 column(1, offset = 3, downloadButton("clinical_save_rscript", "R script", style = "color: #fff; background-color: #62c18b; border-color: #62c18b"))
                                               ),
                                               hr(), uiOutput("nav_7_ui")
                                      )
                          )
                        ),
                        
                        # display metadata --------------------------------------------------------
                        
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title = "Data", 
                                     br(), 
                                     fluidRow(
                                       column(2, tertiary_button("reset", div("Reset", help_button("Reset the dataset to its original downloaded state.", placement = "bottom")))),
                                       column(2, offset = 0, tertiary_button("undo", div("Undo", help_button("Undo the last action.", placement = "bottom")))),
                                       #warning about merged datasets using dates as the criteria
                                       column(6, offset = 2, uiOutput("mergedWarning"))
                                     ), 
                                     br(), br(), 
                                     bsAlert("alert"), withSpinner(DTOutput("dataset"), type = 5)
                            ),
                            tabPanel("Graphical Summary",
                                     colorSelectorInput("clinical_plot_color", "Color of bars:", choices = c(brewer.pal(11, "RdYlBu"), "#808080", "#000000"), ncol = 13),
                                     sliderInput("clinical_binwidths", "Width of bars (for numeric):", min = 0, max = 3000, value = 30),
                                     uiOutput("plots")
                            )
                          ) #tab panel in main panel
                        ) #main panel
                      ) #sidebar layout
             ), #metadata tab panel
             
             # expression data ---------------------------------------------------------
             
             tabPanel(title = "Expression data",
                      #tabsetPanel(id = "expressionPanel",
                                  #tabPanel("1",
                                           sidebarLayout(
                                             sidebarPanel(
                                               h4("Formatting the expression data"),
                                               p("This portion of the application can reformat the expression (assay) data
                                                 associated with the clinical data for the specified GEO ID. If you have already
                                                 loaded clinical data, please start by clicking the button below."),
                                               primary_button("download_expr", "Load Expression Data", 
                                                              icon = help_button("Please make sure to download some clinical data first.")),
                                               hr(),
                                               uiOutput("exprLabels"),
                                               uiOutput("summarizeOptions"),
                                               #uiOutput("transposeCheckbox"),
                                               
                                               checkboxInput(inputId = "transposeExpr", 
                                                             label = div("Transpose the data", 
                                                                         help_button("Values in the ID column become the column names and column names become the ID column."))),
                                               br(),
                                               fluidRow(
                                                 column(1, tertiary_button(id = "undoEvalExpr", label = "Undo")),
                                                 column(1, offset = 2, tertiary_button(id = "resetExpr", label = "Reset")),
                                                 column(1, offset = 3, primary_button(id = "previewExpr", label = "Evaluate"))
                                               ),
                                               hr(),
                                               radioButtons("expression_fileType", "File type:", 
                                                            choices = c("Comma-separated file" = "csv", "Tab-separated file" = "tsv", 
                                                                        "JSON" = "JSON", "Excel" = "xlsx")),
                                               uiOutput("expression_nameFile"),
                                               tags$b("Download:"),
                                               fluidRow(
                                                 column(1, downloadButton("expression_downloadData", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                 column(1, offset = 3, downloadButton("expression_downloadRscript", "R script", style = "color: #fff; background-color: #62c18b; border-color: #62c18b"))
                                               )
                                             ),
                                             mainPanel(
                                               tabsetPanel(
                                                 tabPanel("Assay data",
                                                          fluidRow(
                                                            column(1, secondary_button(id = "expression_prev_cols", label = div(icon("arrow-left"), "Previous columns"))),
                                                            column(1, offset = 8, secondary_button(id = "expression_next_cols", label = div("Next columns", icon("arrow-right"))))
                                                          ),
                                                          withSpinner(dataTableOutput("exprPreview"), type = 5),
                                                          primary_button("expression_evaluate_filters", label = "Evaluate filters")
                                                          ),
                                                 tabPanel("Feature data",
                                                          withSpinner(dataTableOutput("featureData"), type = 5),
                                                          primary_button("feature_evaluate_filters", label = "Evaluate filters")
                                                          ),
                                                 tabPanel("Graphical summary",
                                                          colorSelectorInput("expr_plot_color", "Color of bars:", choices = c(brewer.pal(11, "RdYlBu"), "#808080", "#000000"), ncol = 13),
                                                          sliderInput("expression_binwidths", "Width of bars:", min = 0, max = 3000, value = 1000),
                                                          uiOutput("histograms_expression")
                                                          )
                                               )
                                               #fluidRow(
                                              #   column(5, textInput("searchBox", "Search ID column:")),
                                              #   column(1, actionButton("searchButton", label = icon("search"))),
                                                 #bottom-aligns the search button 
                                                 #https://stackoverflow.com/questions/28960189/bottom-align-a-button-in-r-shiny
                                               #  tags$style(type='text/css', "#searchButton { margin-top: 25px;}")
                                               #),
                                               #withSpinner(DTOutput("expressionData"), type = 5),
                                             ) #main panel
                                           ) #sidebar layout
                                  #)
                      #)
             ), # expression data tab panel
             tabPanel(title = "FAQ",
                      includeMarkdown("www/FAQ.md")
             )
  ) #master panel
) #fluidPage

# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  #look for setting to get it do not disconnect
  #session$allowReconnect(TRUE)

# reactive values ---------------------------------------------------------

  
  values <-
    reactiveValues(
      allData = NULL,
      default_metaData = data.frame("Please enter a GSE ID"),
      metaData = NULL,
      origData = NULL,
      lastData = NULL,
      default_expr_data = data.frame("Please download some data"),
      orig_expr = NULL,
      expr_data = NULL,
      expr_to_display = NULL,
      default_ft_data = data.frame("Please download some data"),
      orig_feature = NULL,
      last_feature = NULL,
      feature_to_display = NULL,
      to_split_selected = FALSE,
      last_selected_rename = NULL,
      last_selected_substitute = NULL,
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
      expression_oFile = "source('geocurateFunctions_User.R')",
      downloadChunkLen = 0,
      currChunkLen = 0,
      expression_downloadChunkLen = 0,
      expression_currChunkLen = 0,
      subAllNums = F,
      expression_id_col = "ID",
      feature_id_col = "ID"
    )
  
  # reset -------------------------------------------------------------------
  
  observeEvent(input$reset, {
    values$metaData <- values$origData
    values$oFile <- removeFromScript(values$oFile, len = values$downloadChunkLen, all = T)
    values$currChunkLen <- 0
  })  
  
  # undo --------------------------------------------------------------------
  
  observeEvent(input$undo, {
    values$metaData <- values$lastData
    values$oFile <- removeFromScript(values$oFile, len = values$currChunkLen)
    values$currChunkLen <- 0
  })  
  
  
  # download & display metaData ---------------------------------------------
  
  observe({
    updateSelectizeInput(
      session = session, 'geoID', server = TRUE,
      choices = data.frame(label = series_list$Accession, value = series_list$Accession, name = series_list$description),
      options = list(render = I(
        '{
          option: function(item, escape) {
            return "<div><strong>" + escape(item.label) + "</strong> " + escape(item.name) + " </div>"
          }
        }'),
        create = TRUE,
        multiple = TRUE,
        maxItems = 5,
        maxOptions = 100
        )
    )
  })
  
  output$gse_link <- renderUI({
    if (!is.null(input$geoID) && !is.na(input$geoID) && !identical(input$geoID, character(0)) && input$geoID != "") { 
      div(a(target = "_blank", href = paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", input$geoID), 
               paste("View", input$geoID, "dataset on GEO")), icon("external-link"))
    }
  })
  
  observeEvent(input$download_data_evaluate, {
    if (is.null(input$geoID) || input$geoID == "") {
      values$errorState <- TRUE
      createAlert(session, "alert", "inputError", title = "Error",
                  content = "Please specify a GSE ID.", append = FALSE)
    }
    else {
      closeAlert(session, "inputError")
      closeAlert(session, "fileError")
      values$errorState <- FALSE
      
      values$allData <- withProgress(downloadClinical(input$geoID, input$download_data_filter, session = session, 
                                               downloadExpr = input$to_download_expression), 
                              message = "Downloading data")
      if (!is.null(values$allData)) {
        closeAlert(session, "fileError")
        values$errorState <- FALSE
        
        platforms <- sapply(values$allData, annotation)
        platform_links <- list()
        for (i in 1:length(unname(platforms))) {
          platform_description <- if (platforms[[i]] %in% platform_list$Accession) 
            platform_list$description[which(platform_list$Accession == platforms[[i]])] else ""
          platform_links[[i]] <- div(a(target = "_blank", href = paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", platforms[[i]]), 
                                       platforms[[i]]), icon("external-link"), 
                                     em(platform_description))
        } 
        
        if (length(platforms) > 0) {
          showModal(modalDialog(radioButtons(inputId = "platformIndex", label = "Which platform file would you like to use?", 
                                             choiceNames = platform_links, 
                                             choiceValues = names(platforms)), 
                                footer = primary_button(id = "usePlatform", label = "Use platform"), size = "s"))
          if (length(platforms) == 1) {
            click("usePlatform")
          }
        }
      } else {
        values$errorState <- TRUE
      }
    }
  })
  
  observeEvent(input$usePlatform, {
    
    removeModal()
    
    extracted_data <- withProgress(processData(values$allData, input$platformIndex, input$download_data_filter, FALSE))
    
    values$metaData <- extracted_data[["metaData"]]
    
    #WRITING COMMANDS TO R SCRIPT
    values$oFile <- "source('geocurateFunctions_User.R')"
    values$oFile <- saveLines(commentify("download metaData"), values$oFile)
    values$oFile <- saveLines(paste0("toFilter <- NULL"), values$oFile)
    values$oFile <- saveLines(paste0("toFilter <- ", format_string(input$download_data_filter)), values$oFile)
    #for (i in 1:length(input$download_data_filter)) {
    #  values$oFile <- saveLines(paste0("toFilter[", i, "] <- ", format_string(input$download_data_filter[i])), values$oFile)
    #}
    values$oFile <- saveLines(paste0("dataSetIndex <- ", format_string(input$platformIndex)), values$oFile)
    values$oFile <- saveLines(c(paste0("geoID <- ", format_string(input$geoID)), "metaData <- downloadClinical(geoID, toFilter, dataSetIndex)"), values$oFile)
    downloadChunkLen <- length(values$oFile)
    
    extracted_data <- NULL
    values$origData <- values$metaData
  })
  
  output$dataset <- DT::renderDT({
    if (!is.null(values$metaData)) {
      closeAlert(session, "fileError")
      datatable(values$metaData, rownames = TRUE)
    }
    else {
      datatable(values$default_metaData, rownames = FALSE, colnames = "NO DATA")
    }
  })
  
  # summary -----------------------------------------------------------------
  
  
  #output$metaSummary <- renderText({printVarsSummary(values$metaData)})
  
  # Create the list of plot names
  plotInput <- reactive({
    if (!is.null(values$metaData)) {
      n_plot <- ncol(values$metaData)
      total_data <- lapply(1:n_plot, function(i){c(table(values$metaData[,i]), Num_NA = sum(is.na(values$metaData[,i])))})
      return(list("n_plot" = n_plot, "total_data" = total_data))
    }
  })
  
  # Create divs
  output$plots <- renderUI({
    
    if (!is.null(values$metaData)) {
      plot_output_list <- lapply(1:plotInput()$n_plot, function(i) {
        #if (!grepl("evalSame", colnames(values$metaData)[i])) {
        plotname <- make.names(colnames(values$metaData)[i])
        plotHeight <- if (isAllNum(values$metaData[i])) 500 else 280
        plotOutput(plotname, height = plotHeight, width = "auto")
        #}
      })   
      do.call(tagList, plot_output_list)
    }
  })
  # Create the actual plots associated with the plot names
  observe({
    if (!is.null(values$metaData)) {
      lapply(1:plotInput()$n_plot, function(i){
        output[[ make.names(colnames(values$metaData)[i]) ]] <- renderPlot({
          #create a histogram if it's numeric, a barplot if it's a factor
          if (isAllNum(values$metaData[i])) {
            hist(as.numeric(as.character(plotInput()$total_data[[i]])), main = colnames(values$metaData)[i], xlab = "Value range", ylab = "Frequency", col = "darkblue", labels = TRUE)
          }
          else {
            if (isAllUnique(values$metaData[i])) {
              barplot(plotInput()$total_data[[i]], main = colnames(values$metaData)[i], xlab = "Values represented in the column", ylab = "Frequency", col = "cornflowerblue", legend.text = "All values are unique.")
              
            }
            else {
              barplot(plotInput()$total_data[[i]], main = colnames(values$metaData)[i], xlab = "Values represented in the column", ylab = "Frequency", col = "cornflowerblue", legend.text = FALSE)
            }
          }
        })
      })
    }
  })
  
  
  # merged warning ----------------------------------------------------------
  
  #output$mergedWarning <- renderUI({
  #  #if any of the values$metaData last column are false, then color yellow and print the warning message
  #  #else, just put a grey box there or nothing at all
  #  if (!is.null(values$metaData) && any(values$metaData[,"evalSame"] == 1)) {
  #   wellPanel(id = "well",
  #              tags$head(tags$style(
  #                HTML('
  #                     #well {
  #                     background-color: #FFFF00;
  #  }'))),
  #              HTML("<p>There is more than one date in this dataset. This could indicate that this is two merged datasets.</p>"))
  #  }
  #})
  
  # extract columns ---------------------------------------------------------
  
  
  #observeEvent(input$to_split, {
  #  updateCheckboxInput(session, inputId = "to_split", label = div("Contains key-value pairs separated by a delimiter",
  #                                                                 icon("caret-up"),
  #                                                                 help_link(id = "split_help")
    #))
    #
    #updateActionButton(session, "to_split", label = icon("caret-square-o-up"))
  #  values$to_split_selected <- if (values$to_split_selected) FALSE else TRUE
  #})
  
  #output$to_split_toggle <- renderUI({
  #  icon_type <- if (values$to_split_selected) "caret-square-o-down" else "caret-square-o-up"
  #  div(actionLink(inputId = "to_split", label = icon(icon_type)), "Contains key-value pairs separated by a delimiter", help_link(id = "split_help"))
  #})
  
  output$choose_cols_to_split <- renderUI({
    #colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    colNames <- colnames(values$metaData)
    checkboxGroupInput(inputId = "cols_to_split", label = "Which columns contain key-value pairs?", colNames)
  })
  
  output$choose_cols_to_divide <- renderUI({
    #colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    colNames <- colnames(values$metaData)
    checkboxGroupInput(inputId = "colsToDivide", label = "Which columns contain multiple values?", colNames)
  })
  
  
  observeEvent(input$reformat_columns, ({
    values$lastData <- values$metaData
    values$metaData <- withProgress(reformat_columns(values$metaData, 
                                                     input$to_split, 
                                                     input$cols_to_split, 
                                                     input$to_divide, 
                                                     input$colsToDivide, 
                                                     input$split_delimiter,
                                                     input$divide_delimiter,
                                                     input$split_all_but,
                                                     input$divide_all_but), message = "Extracting columns")
    updateCheckboxInput(session, inputId = "to_split", value = FALSE)
    updateCheckboxInput(session, inputId = "to_divide", value = FALSE)
    #WRITING COMMANDS TO R SCRIPT
    before <- length(values$oFile)
    values$oFile <- saveLines(commentify("extract values from columns with delimiter"), values$oFile)
    values$oFile <- saveLines(paste0("cols_to_split <- ", format_string(input$cols_to_split)), values$oFile)
    #values$oFile <- saveLines(paste0("cols_to_split <- NULL"), values$oFile)
    #for (i in 1:length(input$cols_to_split)) {
    #  values$oFile <- saveLines(paste0("cols_to_split[", i, "] <- ", format_string(input$cols_to_split[i])), values$oFile)
    #}
    values$oFile <- saveLines(paste0("colsToDivide <- ", format_string(input$cols_to_split)), values$oFile)
    #values$oFile <- saveLines(paste0("colsToDivide <- NULL"), values$oFile)
    #for (i in 1:length(input$colsToDivide)) {
    #  values$oFile <- saveLines(paste0("colsToDivide[", i, "] <- ", format_string(input$colsToDivide[i])), values$oFile)
    #}
    values$oFile <- saveLines(c(paste0("toSplit <- ", format_string(input$to_split)), paste0("to_divide <- ", format_string(input$to_divide)),
                                paste0("split_delimiter <- ", format_string(input$split_delimiter)), 
                                paste0("divide_delimiter <- ", format_string(input$divide_delimiter)),
                                paste0("split_all_but <- ", format_string(input$split_all_but)), 
                                paste0("divide_all_but <- ", format_string(input$divide_all_but)),
                                "metaData <- reformat_columns(metaData, toSplit, cols_to_split, to_divide, colsToDivide, split_delimiter, divide_delimiter, split_all_but, divide_all_but)"), 
                              values$oFile)
    values$currChunkLen <- length(values$oFile) - before
    
    updatePrettyToggle(session, "to_split", value = FALSE)
    updatePrettyToggle(session, "to_divide", value = FALSE)
  }))
  
  
  # filter columns ----------------------------------------------------------
  
  
  output$display_vars_to_keep <- renderUI({
    #colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    colNames <- colnames(values$metaData)
    checkboxGroupInput(inputId = "varsToKeep", label = div("Which columns would you like to keep?",
                                                           help_button("This will drop any unselected columns from the dataset.")), 
                       choices = colNames, selected = colNames)
  })
  
  observeEvent(input$clinical_evaluate_filters, ({
    if (!is.null(values$metaData)) {
      values$lastData <- values$metaData
      values$metaData <- filterCols(values$metaData, input$varsToKeep, input$keep_all_but)
      
      #WRITING COMMANDS TO R SCRIPT
      before <- length(values$oFile)
      values$oFile <- saveLines(commentify("exclude undesired columns"), values$oFile)
      values$oFile <- saveLines(paste0("varsToKeep <- ", format_string(input$varsToKeep)), values$oFile)
      #values$oFile <- saveLines(paste0("varsToKeep <- NULL"), values$oFile)
      #for (i in 1:length(input$varsToKeep)) {
      #  values$oFile <- saveLines(paste0("varsToKeep[", i, "] <- ", format_string(input$varsToKeep[i])), values$oFile)
      #}
      values$oFile <- saveLines(c(paste0("keep_all_but <- ", format_string(input$keep_all_but)),
                                  "metaData <- filterCols(metaData, varsToKeep, keep_all_but)"), 
                                values$oFile)
      values$currChunkLen <- length(values$oFile) - before
    }
  }))
  
  
  # rename columns ----------------------------------------------------------
  
  output$display_cols_to_rename <- renderUI({
    #colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    colNames <- colnames(values$metaData)
    setNames(colNames, colNames)
    selectInput(inputId = "colsToRename", 
                label = "Which column would you like to rename?", 
                choices = colNames, 
                selected = values$last_selected_rename)
  })
  
  observeEvent(input$rename, ({
    values$last_selected_rename <- input$rename_new_name
    values$lastData <- values$metaData
    new_name <- list(input$rename_new_name)
    names(new_name) <- input$colsToRename
    values$metaData <- renameCols(values$metaData, new_name, session)
    
    #WRITING COMMANDS TO R SCRIPT
    before <- length(values$oFile)
    values$oFile <- saveLines(commentify("rename columns"), values$oFile)
    values$oFile <- saveLines(c(paste0("newNames <- list(", format_string(input$rename_new_name), ")"),
                                "names(newNames) <- ", format_string(input$colsToRename)), 
                              values$oFile)
    values$oFile <- saveLines("metaData <- renameCols(metaData, newNames)", values$oFile)
    values$currChunkLen <- length(values$oFile) - before
    
    values$newNames <- NULL
  }))
  
  
  # substitute vals ---------------------------------------------------------
  
  output$input_sub_range <- renderUI({
    
    if (isAllNum(values$metaData[which(colnames(values$metaData) == input$colsToSub)])) {
      output = tagList()
      currCol <- as.numeric(as.character(currCol <- values$metaData[!is.na(values$metaData[,input$colsToSub]),input$colsToSub]))
      output[[1]] <- sliderInput(inputId = "slideInSub", label = "", min = min(currCol), max = max(currCol), value = c(quantile(currCol)[2], quantile(currCol)[3]))
      #output[[2]] <- textInput("newRangeVal", label = "Please enter a value to replace all values in the range:")
      output[[3]] <- tertiary_button("add_val_to_sub", "Add")
      #output[[4]] <- tertiary_button("remove_val_to_sub", "Remove")
      output
    }
    else {
      p(style = "color:red", "Looks like this column isn't numeric!")
    }
    
  })
  
  output$display_cols_to_sub <- renderUI({
    #colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    colNames <- colnames(values$metaData)
    setNames(colNames, colNames)
    selectInput(inputId = "colsToSub", label = div("Please select a column with values to substitute: ", 
                                                   help_link(id = "substitute_help")), 
                choices = colNames,
                selected = values$last_selected_substitute)
  })
  
  observeEvent(input$colsToSub, {
    values$suggestions <- if (input$colsToSub != "" && (input$colsToSub %in% colnames(values$metaData))) 
      unique(as.character(values$metaData[,input$colsToSub]))
  })
  
  output$input_subs_table <- renderRHandsontable({
    rhandsontable(values$DFOut, width = 250, height = 300, stretchH = "all", rowHeaders = FALSE) %>% 
      hot_col(col = "To_Replace", type = "autocomplete", source = values$suggestions, strict = FALSE) #%>%
      #hot_col(col = "New_Val", type = "autocomplete", source = values$thes_suggest_vals, strict = FALSE)
  })
  
  #output$display_user_subs <- renderDT({
  #  if (!is.null(input$colsToSub) && !is.null(values$tablesList[[input$colsToSub]])) 
  #    values$tablesList[[input$colsToSub]] else values$DFOut
  #}, rownames = FALSE, options = list(dom = "t"))
  
  observeEvent(input$input_subs_table, {
    values$DFIn <- hot_to_r(input$input_subs_table)
  }, ignoreInit = T, ignoreNULL = T)
  
  #eventReactive(input$colsToSub, {
  #  currentCol <- input$colsToSub
  #  if (!("To_Replace" %in% colnames(values$tablesList[[currentCol]]))) {
  #    values$tablesList[[currentCol]] <- data.frame(To_Replace = "", stringsAsFactors = FALSE)
  #    values$tablesList[[currentCol]] <- cbind(values$tablesList[[currentCol]], New_Val = "")
  #  }
  #  values$DFOut <- values$tablesList[[currentCol]]
  #}) 
  
  if (FALSE) {
  observeEvent(input$add_val_to_sub, {
    if (!is.null(input$colsToSub) && input$colsToSub != "") {
      #format the table correctly according to the colsToSub selected
      currentCol <- input$colsToSub
      currentDF <- values$tablesList[[currentCol]]
      if (all(currentDF["To_Replace"] == "")) {
        values$tablesList[[currentCol]] <- data.frame(
          To_Replace = if (input$substitute_isrange) paste("RANGE:", paste(input$slideInSub, collapse = "-")) else values$DFIn["To_Replace"], 
          stringsAsFactors = FALSE)
        values$tablesList[[currentCol]] <- cbind(values$tablesList[[currentCol]], 
                                                 New_Val = if (input$substitute_isrange) input$newRangeVal else values$DFIn["New_Val"], stringsAsFactors = F)
      }
      #if the values are not there, add them to the end of the table
      else if ((values$DFIn[["To_Replace"]] %in% currentDF[["To_Replace"]]) || 
              (paste("RANGE:", paste(input$slideInSub, collapse = "-")) %in% currentDF[["To_Replace"]])) {
        if (input$substitute_isrange) {
          currentDF[which(paste("RANGE:", paste(input$slideInSub, collapse = "-")) %in% currentDF[["To_Replace"]]),] <- 
            c(paste("RANGE:", paste(input$slideInSub, collapse = "-")), input$newRangeVal)
        }
        else {
          currentDF[which(currentDF[["To_Replace"]] == values$DFIn[["To_Replace"]]),] <- values$DFIn[1,]
        }
        values$tablesList[[currentCol]] <- currentDF
      }
      else {
        values$tablesList[[currentCol]] <- rbind(values$tablesList[[currentCol]], 
                                                 if (input$substitute_isrange) c(paste("RANGE:", paste(input$slideInSub, collapse = "-")), 
                                                                     input$newRangeVal) else values$DFIn[1,])
      }
    }
  })
  }
  
  observeEvent(input$add_val_to_sub, {
    if (!is.null(input$colsToSub) && input$colsToSub != "") {
      if (all(values$DFIn["To_Replace"] == "")) {
        values$DFIn["To_Replace"] <- paste("RANGE:", paste(input$slideInSub, collapse = "-"))
      } else {
        values$DFIn <- rbind(values$DFIn, c(paste("RANGE:", paste(input$slideInSub, collapse = "-")), ""))
      }
      values$DFOut <- values$DFIn
    }
  })
  
  #observeEvent(input$remove_val_to_sub, {
  #  if (!is.null(input$colsToSub) && !is.null(values$tablesList[[input$colsToSub]])) {
  #    if (!identical(values$tablesList[[input$colsToSub]][,"To_Replace"], character(0)) && values$tablesList[[input$colsToSub]][,"To_Replace"] != "") {
  #      toRemove <- 1
  #      if (!is.null(input$hotOut_rows_selected)) {
  #        toRemove <- input$hotOut_rows_selected
  #      }
  #      values$tablesList[[input$colsToSub]] <- values$tablesList[[input$colsToSub]][-toRemove,]
  #      values$DFOut <- values$tablesList[[input$colsToSub]]
  #    }
  #    if (identical(values$tablesList[[input$colsToSub]][,"To_Replace"], character(0))) {
  #      values$tablesList[[input$colsToSub]][1,] <- c("", "")
  #      values$DFOut <- values$tablesList[[input$colsToSub]]
  #    }
  #  }
  #})
  
  observeEvent(input$evaluate_subs, {
    #if (!identical(values$tablesList, list())) {
    #print("tablesList")
    #print(values$tablesList)
    values$last_selected_substitute <- input$colsToSub
    sub_specs <- list(values$DFIn)
    names(sub_specs) <- input$colsToSub
    #print("DFIn")
    #print(sub_specs)
      values$lastData <- values$metaData
      values$metaData <- withProgress(substitute_vals(values$metaData, #values$tablesList
                                                     sub_specs))
      
      #WRITING COMMANDS TO R SCRIPT
      #before <- length(values$oFile)
      #values$oFile <- saveLines(commentify("substitute values"), values$oFile)
      #values$oFile <- saveLines(paste0("tablesList <- NULL"), values$oFile)
      #for (i in 1:length(values$tablesList)) {
      #  values$oFile <- saveLines(paste0("tablesList[[", format_string(names(values$tablesList)[i]), "]] <- ",
      #                                   "data.frame(", colnames(values$tablesList[[i]])[1], "=c(", 
      #                                   paste(format_string(as.character(values$tablesList[[i]][,1])), collapse = ", "), "), ",
      #                                   colnames(values$tablesList[[i]])[2], "=c(", 
      #                                   paste(format_string(as.character(values$tablesList[[i]][,2])), collapse = ", "), "))"), values$oFile)
      #}
      
      #WRITING COMMANDS TO R SCRIPT
      before <- length(values$oFile)
      values$oFile <- saveLines(commentify("substitute values"), values$oFile)
      values$oFile <- saveLines(paste0("sub_specs <- NULL"), values$oFile)
      for (i in 1:length(values$tablesList)) {
        values$oFile <- saveLines(paste0("sub_specs[[", format_string(input$colsToSub), "]] <- ",
                                         "data.frame(", colnames(values$DFIn)[1], "=c(", 
                                         paste(format_string(as.character(values$DFIn[,1])), collapse = ", "), "), ",
                                         colnames(values$DFIn)[2], "=c(", 
                                         paste(format_string(as.character(values$DFIn[,2])), collapse = ", "), "))"), values$oFile)
      }
      values$oFile <- saveLines("metaData <- substitute_vals(metaData, tablesList)", 
                                values$oFile)
      values$currChunkLen <- length(values$oFile) - before
      
      values$tablesList <- list()
      values$DFOut <- data.frame(To_Replace = "", New_Val = "", stringsAsFactors = FALSE)
      values$suggestions <- unique(as.character(values$metaData[,input$colsToSub]))
      
    #}
  })
  
  # exclude vals ------------------------------------------------------------
  
  
  output$sliderExclude <- renderUI({
    
    if (isAllNum(values$metaData[input$col_valsToExclude])) {
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
  
  output$display_cols_for_exclude <- renderUI({
    #colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    colNames <- colnames(values$metaData)
    setNames(colNames, colNames)
    selectInput(inputId = "col_valsToExclude", label = div("Please select a column with values to exclude: ", 
                                                           help_link(id = "exclude_help")), 
                choices = colNames)
  })
  
  output$display_vals_to_exclude <- renderUI({
    valNames <- unique(as.character(values$metaData[,input$col_valsToExclude]))
    valNames[which(is.na(valNames))] <- "NA"
    checkboxGroupInput(inputId = "valsToExclude", 
                       label = div("Which variables would you like to exclude?", 
                                   help_button("Excluding a variable will remove the entire row that contains that variable.")),
                       choices = valNames)
  })
  
  observe({
    input$valsToExclude
    currentCol <- input$col_valsToExclude
    if (!is.null(currentCol)) {
      if (input$exclude_isrange) {
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
  
  observeEvent(input$clinical_evaluate_exclude, {
    
    values$lastData <- values$metaData
    values$metaData <- withProgress(excludeVars(values$metaData, values$excludesList))
    
    #WRITING COMMANDS TO R SCRIPT
    before <- length(values$oFile)
    values$oFile <- saveLines(commentify("exclude undesired samples"), values$oFile)
    values$oFile <- saveLines(paste0("excludesList <- NULL"), values$oFile)
    for (i in 1:length(values$excludesList)) {
      values$oFile <- saveLines(paste0("excludesList[[", format_string(names(values$excludesList)[i]), "]] <- ", 
                                       format_string(values$excludesList[[i]])), values$oFile)
    }
    values$oFile <- saveLines("metaData <- excludeVars(metaData, excludesList)", 
                              values$oFile)
    values$currChunkLen <- length(values$oFile) - before
    
    values$excludesList <- list()
  })
  
  
  
  # download data -----------------------------------------------------------
  
  output$clinical_display_filename <- renderUI({
    textInput("clinical_user_filename", label = div("File name: ", help_button("If you are downloading an R script, this will make sure the script knows what to name the data file.")), 
              value = paste0(input$geoID, "_Annotations.", input$clinical_file_type))
  })
  
  output$clinical_evaluate_save <- downloadHandler(
    filename = function() {
      input$clinical_user_filename
    },
    content = function(file) {
      myData <- values$metaData
      #myData <- myData[-which(grepl("evalSame", colnames(myData)))]
      myData <- cbind(rownames(myData), myData)
      colnames(myData)[1] <- ""
      
      if (input$clinical_file_type == "csv") {
        write.csv(myData, file, row.names = FALSE, col.names = TRUE)
      }
      else if (input$clinical_file_type == "tsv") {
        write.table(myData, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
      }
      else if (input$clinical_file_type == "JSON") {
        library(jsonlite)
        library(readr)
        
        myData %>% toJSON() %>% write_lines(file)
      }
      else if (input$clinical_file_type == "xlsx") {
        library(xlsx)
        
        write.xlsx(myData, file, row.names = FALSE, showNA = FALSE)
      }
    }
  )
  
  output$clinical_save_rscript <- downloadHandler(
    filename = function() {
      paste0(input$clinical_user_filename, ".R")
    },
    content = function(file) {
      #WRITING COMMANDS TO R SCRIPT
      before <- length(values$oFile)
      values$oFile <- saveLines(commentify("save data"), values$oFile)
      values$oFile <- saveLines(c("metaData <- cbind(rownames(metaData), metaData)", "colnames(metaData)[1] <- ''", 
                                  paste0("file <- ", format_string(input$clinical_user_filename))), values$oFile)
      
      if (input$clinical_file_type == "csv") {
        values$oFile <- saveLines(paste0("write.csv(metaData, file, row.names = FALSE, col.names = TRUE)"), values$oFile)
      }
      else if (input$clinical_file_type == "tsv") {
        values$oFile <- saveLines("write.table(metaData, file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)", 
                                  values$oFile)
      }
      else if (input$clinical_file_type == "JSON") {
        values$oFile <- saveLines(c("library(jsonlite)", "library(readr)", 
                                    "metaData %>% toJSON() %>% write_lines(file)"), 
                                  values$oFile)
      }
      else if (input$clinical_file_type == "xlsx") {
        values$oFile <- saveLines(c("library(xlsx)", "write.xlsx(metaData, file, row.names = FALSE, showNA = FALSE)"), 
                                  values$oFile)
      }
      
      values$currChunkLen <- values$currChunkLen + (length(values$oFile) - before)
      
      saveToRscript(values$oFile, file)
    }
  )
  
  # navigation --------------------------------------------------------------
  
  #1  
  output$nav_1_ui <- renderUI({
    fluidRow(
      column(2, offset = 8, secondary_button('nav_1_to_2_button', 'Next'))
    )
  })
  observeEvent(input$nav_1_to_2_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '2')
  })
  #2  
  output$nav_2_ui <- renderUI({
    fluidRow(
      column(1, tertiary_button('nav_2_to_1_button', 'Back')),
      column(2, offset = 7, secondary_button('nav_2_to_3_button', 'Next'))
    )
  })
  observeEvent(input$nav_2_to_1_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '1')
  })
  observeEvent(input$nav_2_to_3_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '3')
  }) 
  #3  
  output$nav_3_ui <- renderUI({
    fluidRow(
      column(1, tertiary_button('nav_3_to_2_button', 'Back')),
      column(2, offset = 7, secondary_button('nav_3_to_4_button', 'Next'))
    )
  })
  observeEvent(input$nav_3_to_2_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '2')
  })
  observeEvent(input$nav_3_to_4_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '4')
  })
  #4  
  output$nav_4_ui <- renderUI({
    fluidRow(
      column(1, tertiary_button('nav_4_to_3_button', 'Back')),
      column(2, offset = 7, secondary_button('nav_4_to_5_button', 'Next'))
    )
  })
  observeEvent(input$nav_4_to_3_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '3')
  })
  observeEvent(input$nav_4_to_5_button, {
    closeAlert(session, "offendingChars")
    updateTabsetPanel(session, 'clinical_side_panel', selected = '5')
  })
  #5
  output$nav_5_ui <- renderUI({
    fluidRow(
      column(1, tertiary_button('nav_5_to_4_button', 'Back')),
      column(2, offset = 7, secondary_button('nav_5_to_6_button', 'Next'))
    )
  })
  observeEvent(input$nav_5_to_4_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '4')
  })
  observeEvent(input$nav_5_to_6_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '6')
  })
  #6
  output$nav_6_ui <- renderUI({
    fluidRow(
      column(1, tertiary_button('nav_6_to_5_button', 'Back')),
      column(2, offset = 7, secondary_button('nav_6_to_7_button', 'Next'))
    )
  })
  observeEvent(input$nav_6_to_5_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '5')
  })
  observeEvent(input$nav_6_to_7_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '7')
  })
  #7
  output$nav_7_ui <- renderUI({
    fluidRow(
      column(1, tertiary_button('nav_7_to_6_button', 'Back')),
      column(2, offset = 7, secondary_button('nav_7_to_expression_button', 'Next'))
    )
  })
  observeEvent(input$nav_7_to_6_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '6')
  })
  observeEvent(input$nav_7_to_expression_button, {
    updateTabsetPanel(session, 'top_level', selected = 'Expression data')
  })
  
  # help modals -------------------------------------------------------------

  
  observeEvent(input$split_help, {
    help_modal("www/Split_Vars_Documentation.md", "split_images")
  })
  output$split_images <- renderUI({
    images <- c("separate_example.gif")
    image_names <- c("Separate Columns Demo")
    
    create_image_grid(images, image_names)
  })
  
  observeEvent(input$divide_help, {
    help_modal("www/Divide_Vars_Documentation.md", "divide_images")
  })
  output$divide_images <- renderUI({
    images <- c("divide_example.gif")
    image_names <- c("Divide Columns Demo")
    
    create_image_grid(images, image_names)
  })
  
  observeEvent(input$substitute_help, {
    help_modal("www/Substitute_Vals_Documentation.md", "substitute_images")
  })
  output$substitute_images <- renderUI({
    images <- c("substitute_example.gif")
    image_names <- c("Substitute Values Demo")
    
    create_image_grid(images, image_names)
  })
  
  observeEvent(input$exclude_help, {
    help_modal("www/Exclude_Vals_Documentation.md", "exclude_images")
  })
  output$exclude_images <- renderUI({
    images <- c("exclude_example.gif")
    image_names <- c("Exclude Values Demo")
    
    create_image_grid(images, image_names)
  })
  
  observeEvent(input$download_help, {
    help_modal("www/Download_Data_Documentation.md", "download_images")
  })
  output$download_images <- renderUI({
    images <- c("download_example_no_filter.gif", "download_example_with_filter.gif")
    image_names <- c("Download Without Filters", "Download With Filters")
    
    create_image_grid(images, image_names)
  })
  
  observeEvent(input$clickimg, {
    showModal(modalDialog({
      tags$img(src = input$clickimg, width = "100%", height = "100%")
    },
    size = "l"
    ))
  }, ignoreInit = TRUE)
  

  # expression data sidebar -------------------------------------------------
   
  observeEvent(input$download_expr, {
    
    if (!is.null(values$allData)) {
      
      extracted_data <- withProgress(processData(values$allData, input$platformIndex, input$download_data_filter, TRUE))
    
      values$orig_expr <- extracted_data[["expressionData"]]
      values$last_expr <- values$orig_expr
      values$expr_data <- values$orig_expr
      if (is.null(values$expr_data)) {
        values$default_expr_data <- data.frame(paste0("No assay data available for ", input$geoID))
        values$default_ft_data <- data.frame(paste0("No feature data available for ", input$geoID))
      } else {
        values$expr_to_display <- advance_columns_view(values$expr_data, start = 1, forward_distance = 5)
        values$orig_feature <- extracted_data[["featureData"]]
        values$last_feature <- values$orig_feature
        values$feature_to_display <- find_intersection(values$last_feature, values$expr_to_display)
        
        values$expression_oFile <- saveLines(commentify("download expression data"), values$expression_oFile)
        values$expression_oFile <- saveLines(paste0("dataSetIndex <- ", format_string(input$platformIndex)), values$expression_oFile)
        values$expression_oFile <- saveLines(c(paste0("geoID <- ", format_string(input$geoID)),
                                               "allData <- downloadExpression(geoID, dataSetIndex)",
                                               "expressionData <- allData[['expressionData']]",
                                               "featureData <- allData[['featureData']]"), 
                                             values$expression_oFile)
        
        values$expression_downloadChunkLen <- length(values$expression_oFile)
      }
    }
  })
  
  output$exprLabels <- renderUI({
    selectInput("colForExprLabels", label = div("Please select a column to replace the expression IDs", 
                                                help_button("To keep the same ID column, please choose ID.")), 
                choices = colnames(values$feature_to_display))
  })
  
  output$summarizeOptions <- renderUI({
    if (!is.null(input$colForExprLabels) && input$colForExprLabels != "" &&
       !input$colForExprLabels %in% findExprLabelColumns(values$feature_to_display)) {
      selectInput("howToSummarize", label = div("It looks like this column contains multiple values for one expression ID.
                  How would you like to summarize the data?", help_button("Groups the data by ID and takes the specified measurement for the group.")), 
                  choices = c("mean", "median", "max", "min", "keep all"))
    }
  })
  
  #output$transposeCheckbox <- renderUI({
  #  textColor <- if_else(!is.null(input$colForExprLabels) && input$colForExprLabels != "" &&
  #                         !input$colForExprLabels %in% findExprLabelColumns(values$feature_to_display) &&
  #                         input$howToSummarize == "keep all",
  #                       "color:gray", "color:black")
  #})
  
  observe({
    if (!is.null(input$colForExprLabels) && input$colForExprLabels != "" &&
       !input$colForExprLabels %in% findExprLabelColumns(values$feature_to_display)) {
      shinyjs::disable("transposeExpr")
    }
  })
  
  observeEvent(input$previewExpr, {
    
    if (!is.null(values$expr_data)) {
      values$last_expr <- values$expr_data
      
      values$expr_data <- withProgress(message = "Replacing the ID column", 
                                           replaceID(values$expr_data, values$feature_to_display, input$colForExprLabels, input$howToSummarize))
      values$feature_id_col <- input$colForExprLabels
      
      before <- length(values$expression_oFile)
      
      #WRITING COMMANDS TO EXPRESSION RSCRIPT
      values$expression_oFile <- saveLines(c(commentify("replace ID column"),
                                             paste0("expressionData <- replaceID(expressionData, featureData, ", 
                                                    format_string(input$colForExprLabels), ", ",
                                                    format_string(input$howToSummarize), ")")), 
                                           values$expression_oFile)
      if (input$transposeExpr) {
        values$expr_data <- withProgress(message = "Transposing the data", 
                                             quickTranspose(values$expr_data))
        
        values$expression_id_col <- "colnames"
        
        #WRITING COMMANDS TO EXPRESSION RSCRIPT
        values$expression_oFile <- saveLines(c(commentify("transpose data"), 
                                               "expressionData <- quickTranspose(expressionData)"), 
                                             values$expression_oFile)
      }
      
      values$expr_to_display <- advance_columns_view(values$expr_data, start = 1, forward_distance = 5)
      
      after <- length(values$expression_oFile)
      
      values$expression_currChunkLen <- after - before
    }
    
    #updateTabsetPanel(session, "expressionPanel", selected = "2")
  })
  
  observeEvent(input$undoEvalExpr, {
    if (!is.null(values$expr_data)) {
      values$feature_to_display <- values$last_feature
      values$expr_data <- values$last_expr
      values$expr_to_display <- advance_columns_view(values$expr_data, start = 1, forward_distance = 5)
      values$expression_oFile <- removeFromScript(values$expression_oFile, len = values$expression_currChunkLen)
      values$expression_currChunkLen <- 0
    }
  })
  
  observeEvent(input$resetExpr, {
    if (!is.null(values$expr_data)) {
      values$feature_to_display <- values$orig_feature
      values$expr_data <- values$orig_expr
      values$expr_to_display <- advance_columns_view(values$expr_data, start = 1, forward_distance = 5)
      values$expression_oFile <- removeFromScript(values$expression_oFile, len = values$expression_downloadChunkLen, all = T)
      values$expression_currChunkLen <- 0
    }
  })
  
  # download expression data -----------------------------------------------------------
  
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
          write.csv(values$expr_data, file, row.names = FALSE, col.names = TRUE)
          incProgress()
        })
      }
      else if (input$expression_fileType == "tsv") {
        withProgress(message = "Writing data to file",
                     write.table(values$expr_data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
      }
      else if (input$expression_fileType == "JSON") {
        library(jsonlite)
        library(readr)
        
        withProgress(message = "Writing data to file", {
          incProgress()
          values$expr_data %>% toJSON() %>% write_lines(file)
          incProgress()
        })
        
      }
      else if (input$expression_fileType == "xlsx") {
        library(xlsx)
        
        withProgress(message = "Writing data to file", {
          incProgress()
          write.xlsx(values$expr_data, file, row.names = FALSE, showNA = FALSE)
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
        values$expression_oFile <- saveLines(paste0("write.csv(expressionData, file, row.names = FALSE, col.names = TRUE)"), values$expression_oFile)
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
      
      saveToRscript(values$expression_oFile, file)
      
      values$expression_currChunkLen <- values$expression_currChunkLen + (length(values$expression_oFile) - before)
    }
  )
  
  # main panel expression data ----------------------------------------------
  
  observeEvent(input$expression_next_cols, {
    next_cols <- advance_columns_view(values$expr_data, start = colnames(values$expr_to_display)[ncol(values$expr_to_display)], forward_distance = 5)
    values$expr_to_display <- if (!is.null(next_cols)) next_cols else values$expr_to_display
  })
  
  observeEvent(input$expression_prev_cols, {
    prev_cols <- retract_columns_view(values$expr_data, last_column = colnames(values$expr_to_display)[1], backward_distance = 5)
    values$expr_to_display <- if (!is.null(prev_cols)) prev_cols else values$expr_to_display
  })
  
  output$exprPreview <- DT::renderDT({
    if (!is.null(values$expr_to_display)) {
      datatable(values$expr_to_display, filter = "top", rownames = FALSE, options = list(dom = "tp"))
    } else {
      datatable(values$default_expr_data, rownames = FALSE, 
                colnames = "NO DATA", options = list(dom = "tp"))
    }
  }) 
  
  observeEvent(input$expression_evaluate_filters, {
    values$last_expr <- values$expr_data
    values$last_feature <- values$feature_to_display
    values$expr_to_display <- filterExpressionData(values$expr_to_display, input$exprPreview_search_columns)
    values$expr_data <- find_intersection(values$expr_data, values$expr_to_display)
    values$feature_to_display <- find_intersection(values$feature_to_display, values$expr_data, values$feature_id_col, values$expression_id_col)
    #WRITING COMMANDS TO EXPRESSION RSCRIPT
    before <- length(values$expression_oFile)
    values$expression_oFile <- saveLines(c(commentify("filter data"),
                                           paste0("filterSpecs <- ", format_string(input$exprPreview_search_columns)),
                                           "expressionData <- filterExpressionData(expressionData, filterSpecs)",
                                           paste0("expressionIdCol <- ", format_string(values$expression_id_col)),
                                           paste0("featureIdCol <- ", format_string(values$feature_id_col)),
                                           "expressionData <- find_intersection(featureData, expressionData, featureIdCol, expressionIdCol)"), 
                                         values$expression_oFile)
    values$expression_currChunkLen <- length(values$expression_oFile) - before
  })
  
  #observeEvent(input$viewFilters, {
  #  print(input$exprPreview_search_columns)
  #})
  
  # main panel feature data -------------------------------------------------

  output$featureData <- DT::renderDT({
    if (!is.null(values$feature_to_display)) {
      datatable(values$feature_to_display, filter = "top", rownames = FALSE, options = list(dom = "tp"))
    }
    else {
      datatable(values$default_ft_data, rownames = FALSE, 
                colnames = "NO DATA", options = list(dom = "tp"))
    }
  }) 
  
  observeEvent(input$feature_evaluate_filters, {
    values$last_feature <- values$feature_to_display
    values$last_expr <- values$expr_data
    values$feature_to_display <- filterExpressionData(values$feature_to_display, input$featureData_search_columns)
    values$expr_data <- find_intersection(values$expr_data, values$feature_to_display, values$expression_id_col, values$feature_id_col)
    values$expr_to_display <- advance_columns_view(values$expr_data, start = 1, forward_distance = 5)
    #WRITING COMMANDS TO EXPRESSION RSCRIPT
    before <- length(values$expression_oFile)
    values$expression_oFile <- saveLines(c(commentify("filter data"),
                                           paste0("filterSpecs <- c(", paste(format_string(input$featureData_search_columns), collapse = ", "), ")"),
                                           "featureData <- filterExpressionData(featureData, filterSpecs)",
                                           paste0("expressionIdCol <- ", format_string(values$expression_id_col)),
                                           paste0("featureIdCol <- ", format_string(values$feature_id_col)),
                                           "expressionData <- find_intersection(expressionData, featureData, expressionIdCol, featureIdCol)"), 
                                         values$expression_oFile)
    values$expression_currChunkLen <- length(values$expression_oFile) - before
  })

  # graphical summary -------------------------------------------------------
  
  #output$expression_summary_histogram <- renderPlot({
  #  
  #})
  #output$expression_summary_boxplot <- renderPlot({
  #  
  #})
  
  histograms_expression_input <- reactive({
    if (!is.null(values$expr_to_display)) {
      n_plot <- ncol(values$expr_to_display)
      total_data <- lapply(2:n_plot, function(i){values$expr_to_display[,i]})
      return(list("n_plot" = n_plot, "total_data" = total_data))
    }
  })
  
  # Create divs
  output$histograms_expression <- renderUI({
    
    if (!is.null(values$expr_to_display)) {
      plot_output_list <- lapply(2:histograms_expression_input()$n_plot, function(i) {
        plotname <- make.names(colnames(values$expr_to_display)[i])
        div(withSpinner(plotOutput(plotname, height = 500, width = "auto"), type = 5), actionButton(paste0("savePlot", i), "save Plot", class = "expression_plot"))
      })   
      do.call(tagList, plot_output_list)
    }
  })
  # Create the actual plots associated with the plot names
  observe({
    if (!is.null(values$expr_to_display)) {
      lapply(2:histograms_expression_input()$n_plot, function(i){
        output[[ make.names(colnames(values$expr_to_display)[i]) ]] <- renderPlot({
          ggplot(data = data.frame(measured = as.numeric(as.character(histograms_expression_input()$total_data[[i - 1]]))), aes(x = measured)) +
            geom_histogram(binwidth = input$expression_binwidths, fill = input$expr_plot_color) +
            labs(x = "Expression",
                 y = "Number of spots") +
            ggtitle(colnames(values$expr_to_display)[i]) +
            theme_bw(base_size = 18)
          #hist(as.numeric(as.character(histograms_expression_input()$total_data[[i - 1]])), main = colnames(values$expr_to_display)[i], xlab = "Expression", ylab = "Number of spots", col = "darkblue", labels = TRUE)
        })
      })
    }
  })
  
  observeEvent(input$last_btn_expression, {
    print(input$last_btn_expression)
  })
  
  
}

shinyApp(ui = ui, server = server)
