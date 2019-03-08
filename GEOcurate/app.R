suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(shinycssloaders)
  library(shinyBS)
  library(plotly)
  library(feather)
  library(shinyjs)
  library(rhandsontable)
  library(shinyWidgets)
  library(RColorBrewer)
  source("geocurateFunctions.R")
})

start_time <- Sys.time()
series_list <- read_feather("www/series_list.feather")
platform_list <- read_feather("www/platform_list.feather")
end_time <- Sys.time()
print(paste("Reading files", end_time - start_time))

# help icon to add as tag to buttons, etc ---------------------------------


help_button <- function(message = "content", placement = "right") {
  tipify(icon("question-circle"), title = message, placement = placement, trigger = "hover")
}

help_link <- function(id) {
  tipify(actionLink(inputId = id, label = icon("question-circle")), title = "Click for help", placement = "right", trigger = "hover")
}

help_modal <- function(help_file, images_id = NULL) {
  showModal(
    modalDialog(
      includeMarkdown(help_file),
      conditionalPanel(
        condition = !is.null(images_id),
        uiOutput(images_id)
      ),
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


primary_button <- function(id, label, icon = NULL, class = NULL, width = NULL) {
  shiny::actionButton(id, div(label, icon), width = width, 
               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", class = class)
}

secondary_button <- function(id, label, icon = NULL, class = NULL, width = NULL) {
  shiny::actionButton(id, div(label, icon), width = width, 
               style = "color: #fff; background-color: #2ca25f; border-color: #2ca25f", class = class)
}

tertiary_button <- function(id, label, icon = NULL, class = NULL, width = NULL) {
  shiny::actionButton(id, div(label, icon), width = width, 
               style = "color: #fff; background-color: #6baed6; border-color: #6baed6", class = class)
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
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    includeScript("reactive_preferences.js"),
  navbarPage(title = "TidyGEO", id = "top_level",
             tabPanel(title = "Choose dataset",
                      
                      sidebarLayout(
                        sidebarPanel(
                          h4("Importing the data"),
                          div("Welcome to TidyGEO! This application allows you to reformat data
                              from ",
                              a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/", "Gene Expression Omnibus,"),
                              " which can then be used to answer research questions. Once you have found a",
                              a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/gds", "series of interest,"),
                              "complete the instructions below."
                          ),
                          selectizeInput(inputId = "geoID", label = div("Please input a GSE ID: ", 
                                                                        help_link(id = "download_help")), choices = NULL),
                          #uiOutput("gse_link"),
                          primary_button(id = "download_data_evaluate", label = "Import"),
                          hr(), uiOutput("start_clinical_nav_ui")
                        ),
                        mainPanel(
                          h4("Series information"),
                          bsAlert("alert"),
                          uiOutput("series_information_description"),
                          htmlOutput("series_information")
                        )
                      )),
             tabPanel(title = "Clinical data",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          useShinyjs(),
                          tabsetPanel(id = "clinical_side_panel",

                                      # exclude vars ------------------------------------------------------------

                                      
                                      tabPanel("1",
                                               h4("Selecting informative columns"),
                                               p("It can be helpful to filter out unneeded columns for better storage capacity and improved
                                                 human readability. Here, you can choose which columns are most important for you to keep
                                                 and drop the rest. You may either use preset filters that will detect commonly-dropped
                                                columns, or select specific columns to keep."),
                                               radioButtons(inputId = "filter_option", label = div("Please select an option:", help_link("filter_help")),
                                                            choices = c("Use preset filters" = "preset_filters",
                                                                        "Select columns by column name" = "column_filters")),
                                               conditionalPanel(condition = "input.filter_option == 'preset_filters'",
                                                                checkboxGroupInput(inputId = "download_data_filter", label = div("Remove columns in which every value...",
                                                                                                                                 help_button("Drops columns that match the selected criteria from the table.")),
                                                                                   choiceNames = list("Is the same", 
                                                                                                      "Is unique",
                                                                                                      "Contains a date", 
                                                                                                      "Is a web address"),
                                                                                   choiceValues = list("same_vals", "all_diff", "dates", "url"))
                                               ),
                                              conditionalPanel(condition = "input.filter_option == 'column_filters'",
                                                               div(tags$b("Choose columns to keep:"),
                                                                   help_button("This will drop unselected columns from the table.")),
                                                               checkboxInput(inputId = "select_all_columns", label = tags$i("Select all"), value = TRUE),
                                                               uiOutput("display_vars_to_keep")
                                              ),
                                              primary_button(id = "clinical_evaluate_filters", label = "Filter columns"),
                                              hr(), uiOutput("nav_1_ui")
                                      ),
                                      
                                      
                                      # split variables ---------------------------------------------------------
                                      
                                      
                                      #specify which columns to split apart, and the delimiter
                                      tabPanel("2",
                                               h4("Formatting the data"),
                                               p("Sometimes columns contain multiple values in them. This makes it so that the values
                                                 cannot be analyzed separately. If you see any columns in your data that contain multiple values, 
                                                 you can indicate that here and separate them."),
                                               checkboxInput(inputId = "to_split", label = div(tags$b("Choose columns with key-value pairs separated by a delimiter"),
                                                                                               help_link(id = "split_help")
                                                                                               )),
                                               conditionalPanel(condition = "input.to_split == true",
                                                                checkboxInput(inputId = "select_all_split", 
                                                                              label = tags$i("Select all")),
                                                                uiOutput("choose_cols_to_split"),
                                                                textInput(inputId = "split_delimiter", label = "Delimiter (including any spaces): ")
                                               ),
                                               checkboxInput(inputId = "to_divide", label = div(tags$b("Choose columns with multiple values in one column"),
                                                                                                help_link(id = "divide_help"))),
                                               conditionalPanel(condition = "input.to_divide == true",
                                                                checkboxInput(inputId = "select_all_divide", 
                                                                              label = tags$i("Select all")),
                                                                uiOutput("choose_cols_to_divide"),
                                                                textInput(inputId = "divide_delimiter", label = "Delimiter (including any spaces): ")
                                               ),
                                               primary_button(id = "reformat_columns", label = "Reformat columns"),
                                               hr(), uiOutput("nav_2_ui")
                                      ),
                                      
                                      # rename columns ----------------------------------------------------------
                                      
                                      
                                      #renaming any columns
                                      tabPanel("3",
                                               h4("Renaming columns"),
                                               p("In order to integrate the data with other data sources or for humans to be able to understand the data,
                                                 it may be helpful to replace the existing column names with more accurate/descriptive ones.
                                                 Here, you can give any column a new name."),
                                               uiOutput("display_cols_to_rename"),
                                               textInput(inputId = "rename_new_name", label = "Please specify a new name for the column."),
                                               primary_button(id = "rename", label = "Rename column"),
                                               hr(), uiOutput("nav_3_ui")
                                      ),
                                      
                                      # substitute --------------------------------------------------------------
                                      
                                      
                                      #specify which values to substitute for other values/which values should be treated as NA
                                      tabPanel("4",
                                               h4("Substituting values"),
                                               p("In order to achieve the uniformity required to combine datasets, it may be helpful to substitute 
                                                 some of the values in the data for other values. Here, you can identify values you would like to replace
                                                 and an alternative to replace them with."),
                                               uiOutput("display_cols_to_sub"),
                                               checkboxInput(inputId = "substitute_isrange", 
                                                             label = div("Specify a range of values to substitute?", 
                                                                         help_button("Helpful for numeric data."))),
                                               conditionalPanel(condition = "input.substitute_isrange == true",
                                                                uiOutput("input_sub_range")),
                                               h5('Right click to add or remove rows.'),
                                               rHandsontableOutput("input_subs_table"),
                                               conditionalPanel(condition = "input.substitute_isrange == false",
                                                                checkboxInput("sub_w_regex", div("Use regex",
                                                                                                 help_link(id = "regex_help"))))
                                               ,
                                               primary_button("evaluate_subs", "Substitute"),
                                               hr(), uiOutput("nav_4_ui")
                                      ),
                                      
                                      # exclude variables -------------------------------------------------------
                                      
                                      
                                      tabPanel("5",
                                               h4("Filtering samples"),
                                               p("You may want to remove some of the values in a column, for example, if you have missing (NA) values.
                                                  Here, you can specify which values you would like to remove.
                                                 Excluding a value will take out the entire row that contains that value in the selected column."),
                                               uiOutput("display_cols_for_exclude"),
                                               checkboxInput("exclude_isrange", 
                                                             label = div("Specify a range of values to exclude?", 
                                                                         help_button("Helpful for numeric data."))),
                                               conditionalPanel(condition = "input.exclude_isrange == true",
                                                                uiOutput("sliderExclude")),
                                               conditionalPanel(condition = "input.exclude_isrange == false",
                                                                div(tags$b("Which variables would you like to exclude?"), help_button("Excluding a variable will remove the entire row that contains that variable.")),
                                                                checkboxInput(inputId = "select_all_exclude", label = tags$i("Select all")),
                                                                uiOutput("display_vals_to_exclude")),
                                               primary_button("clinical_evaluate_exclude", "Exclude"),
                                               hr(), uiOutput("nav_5_ui")
                                      ),

                                      # save clinical data ------------------------------------------------------

                                      
                                      tabPanel("6",
                                               h4("Saving the data"),
                                               p("Here is where you can download the clinical data to your computer. 
                                                 You can also download the R script that produced this data. The R script allows you
                                                 to replicate the steps you took so you can see how the data was obtained."),
                                               radioButtons("clinical_file_type", div("File type:", help_link("clinical_files_help")), 
                                                            choices = c("Comma-separated file" = "csv", "Tab-separated file" = "tsv", 
                                                                        "JSON" = "JSON", "Excel" = "xlsx")),
                                               uiOutput("clinical_display_filename"),
                                               tags$b("Download:"),
                                               fluidRow(
                                                 column(1, downloadButton("clinical_evaluate_save", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                 column(7, offset = 3, div(downloadButton("clinical_save_rscript", "R script", 
                                                                                      style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                                                                           help_link(id = "clinical_r_help")))
                                               ),
                                               hr(), uiOutput("nav_6_ui")
                                      )
                          )
                        ),
                        
                        # display metadata --------------------------------------------------------
                        
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title = "Clinical Data",
                                     h4("Clinical Data"),
                                     p("A description of the biological samples and protocols to which they were subjected."),
                                     br(), 
                                     fluidRow(
                                       column(2, tertiary_button("reset", div("Reset", help_button("Reset the dataset to its original downloaded state.", placement = "bottom")))),
                                       column(2, offset = 0, tertiary_button("undo", div("Undo", help_button("Undo the last action.", placement = "bottom")))),
                                       #warning about merged datasets using dates as the criteria
                                       column(6, offset = 2, uiOutput("mergedWarning"))
                                     ), 
                                     br(), br(), 
                                     bsAlert("parseError"),
                                     withSpinner(DTOutput("dataset"), type = 5)
                            ),
                            tabPanel("Graphical Summary",
                                     colorSelectorInput("clinical_plot_color", "Color of bars:", choices = c(brewer.pal(11, "RdYlBu"), "#808080", "#000000"), ncol = 13),
                                     uiOutput("choose_variable_to_view"),
                                     sliderInput("clinical_binwidths", "Width of bars (for numeric):", min = 0, max = 60, value = 30),
                                     uiOutput("plots")
                            )
                          ) #tab panel in main panel
                        ) #main panel
                      ) #sidebar layout
             ), #metadata tab panel
             
             # expression data ---------------------------------------------------------
             
             tabPanel(title = "Assay data",
                      
                      sidebarLayout(
                        sidebarPanel(
                          tabsetPanel(id = "expression_side_panel",
                                      tabPanel("1",
                                               h4("Formatting the assay data"),
                                               p("This portion of the application can reformat the assay data
                                           associated with the specified GEO ID. If you have already
                                           loaded the series data, please start by clicking the buttons below."),
                                               tags$b("Options:"),
                                               fluidRow(
                                                 column(12, 
                                                        primary_button("expression_replace_id", 
                                                                       label = div(icon("exchange-alt"),
                                                                                   "Use different column ID"),
                                                                       width = '200px', class = "indent"), 
                                                        help_link(id = "replace_id_help"))
                                                 
                                               ),
                                               fluidRow(
                                                 column(12, 
                                                        primary_button(id = "expression_transpose",
                                                                       label = div(icon("retweet"),
                                                                                   "Transpose"),
                                                                       width = '200px', class = "indent"),
                                                        help_link(id = "transpose_help"))
                                               ),
                                               primary_button("expression_evaluate_filters", 
                                                              label = div(icon("filter"),
                                                                          "Apply filters"),
                                                              width = '200px', class = "indent"),
                                               help_link(id = "evaluate_filters_help"),
                                               div(
                                                 #column(2, primary_button(id = "previewExpr", label = "Update")),
                                                 tertiary_button(id = "undoEvalExpr", label = "Undo"),
                                                 tertiary_button(id = "resetExpr", label = "Reset", class = "right_align")
                                               ),
                                               hr(), uiOutput("expression_nav_1_ui")
                                      ), #tabPanel1
                                      tabPanel("2",
                                               h4("Saving the data"),
                                               p("Here is where you can download the clinical data to your computer. 
                                                 You can also download the R script that produced this data. The R script allows you
                                                 to replicate the steps you took so you can see how the data was obtained."),
                                               radioButtons("expression_fileType", div("File type:", help_link("expression_files_help")), 
                                                            choices = c("Comma-separated file" = "csv", "Tab-separated file" = "tsv", 
                                                                        "JSON" = "JSON", "Excel" = "xlsx")),
                                               uiOutput("expression_nameFile"),
                                               tags$b("Download:"),
                                               fluidRow(
                                                 column(1, downloadButton("expression_downloadData", "Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                 column(7, offset = 3, div(downloadButton("expression_downloadRscript", "R script", 
                                                                                          style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                                                                           help_link(id = "expression_r_help")))
                                               ),
                                               hr(), uiOutput("expression_nav_2_ui")
                                      ) #tabPanel2
                              ) #tabsetPanel
                          ), #sidebarPanel
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Assay data",
                                     h4("Assay Data"),
                                     p("The abundance measurement of each element derived from each sample."),
                                     br(),
                                     div(
                                       secondary_button(id = "expression_prev_cols", label = div(icon("arrow-left"), "Previous columns")),
                                       secondary_button(id = "expression_next_cols", label = "Next columns", icon = icon("arrow-right"), class = "right_align")
                                     ),
                                     bsAlert("alpha_alert"),
                                     withSpinner(dataTableOutput("exprPreview"), type = 5)#,
                                     #primary_button("expression_evaluate_filters", label = "Evaluate filters")
                            ),
                            tabPanel("Graphical summary",
                                     colorSelectorInput("expr_plot_color", "Color of bars:", choices = c(brewer.pal(11, "RdYlBu"), "#808080", "#000000"), ncol = 13),
                                     uiOutput("expr_select_binwidths"),
                                     #checkboxInput("expr_display_labels", "Display labels above columns?"),
                                     uiOutput("histograms_expression")
                            )
                          )
                        ) #main panel
                      ) #sidebar layout
             ), # expression data tab panel
             tabPanel(title = "FAQ",
                      includeMarkdown("www/FAQ.md")
             )
  ) #master panel
) #fluidPage

# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  #look for setting to get it do not disconnect
  session$allowReconnect(TRUE)
  session$onSessionEnded(stopApp)

# reactive values ---------------------------------------------------------

  
  values <-
    reactiveValues(
      allData = NULL,
      default_metaData = data.frame("Please load some clinical data"),
      metaData = NULL,
      origData = NULL,
      lastData = NULL,
      default_expr_data = data.frame("Please load some assay data"),
      orig_expr = NULL,
      expr_data = NULL,
      expr_to_display = NULL,
      default_ft_data = data.frame("Please load some assay data"),
      orig_feature = NULL,
      last_feature = NULL,
      feature_data = NULL,
      feature_to_display = NULL,
      to_split_selected = FALSE,
      last_selected_rename = NULL,
      last_selected_substitute = NULL,
      last_selected_exclude = NULL,
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
      oFile = commentify(" "),
      expression_oFile = "source('geocurateFunctions_User.R')",
      downloadChunkLen = 0,
      currChunkLen = 0,
      expression_downloadChunkLen = 0,
      expression_currChunkLen = 0,
      subAllNums = F,
      expression_id_col = "ID",
      feature_id_col = "ID",
      clinical_plot_to_save = NULL,
      expr_plot_to_save = NULL,
      expression_disable_btns = FALSE,
      expression_warning_state = FALSE,
      display_barwidth_option = FALSE
    )
  
  get_series_information <- function() {
    if (!is.null(input$geoID) && input$geoID != "") {
      geo_url <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", input$geoID)
    } else {
      geo_url <- "https://www.ncbi.nlm.nih.gov/gds/"
    }
    return(tags$iframe(src = geo_url, style = "width:100%;",
                       frameborder = "0",
                       id = "iframe", 
                       height = "500px"))
  }
  
  output$series_information <- renderUI({
    get_series_information()
  })
  
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
    start_time <- Sys.time()
    updateSelectizeInput(
      session = session, 'geoID', server = TRUE,
      choices = series_list,
      options = list(render = I(
        '{
          option: function(item, escape) {
            return "<div><strong>" + escape(item.label) + "</strong> " + escape(item.name) + " </div>"
          }
        }'),
        create = TRUE,
        multiple = FALSE,
        maxItems = 5,
        maxOptions = 100
        )
    )
    end_time <- Sys.time()
    print(paste("Populating dropdown", end_time - start_time))
  })
  
  output$series_information_description <- renderUI({
    if (!is.null(input$geoID)) {
      div(
        p(
          paste0("Here is the webpage for ", input$geoID, ", where you can preview some information about the
          dataset.")), "To view this webpage in a separate browser, click ",
        a(target = "_blank", href = paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", input$geoID), 
          " here."), icon("external-link"))
    } else {
      p("Please enter a GEO ID to preview some information about the dataset here.")
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
      
      platforms <- get_platforms(input$geoID, session)
      if (!is.null(platforms)) {
        platform_links <- list()
        for (i in 1:length(platforms)) {
          id <- str_remove(platforms[i], "GSE\\d+-")
          id <- str_remove(id, "_series_matrix.txt.gz")
          platform_description <- if (any(str_detect(platform_list$Accession, id)))
            platform_list$description[which(platform_list$Accession == id)] else ""
          platform_links[[i]] <- div(a(target = "_blank", href = paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", id), 
                                       id), icon("external-link"), 
                                     em(platform_description))
        }
        
        if (length(platforms) > 0) {
          showModal(modalDialog(radioButtons(inputId = "platformIndex", label = "Which platform file would you like to use?", 
                                             choiceNames = platform_links, 
                                             choiceValues = platforms), 
                                footer = primary_button(id = "usePlatform", label = "Use platform"), size = "s",
                                easyClose = TRUE))
          if (length(platforms) == 1) {
            click("usePlatform")
          }
        }
      }
    }
  })
  
  observeEvent(input$usePlatform, {
    print(input$platformIndex)
    
    removeModal()
    
    values$allData <- withProgress(
      load_series(input$geoID, input$platformIndex, session = session), 
      message = "Downloading series data from GEO")
    values$metaData <- NULL
    values$exprData <- NULL
    
    #WRITING COMMANDS TO R SCRIPT
    values$oFile <- saveLines(commentify("load series"), values$oFile)
    values$oFile <- saveLines(paste0("dataSetIndex <- ", format_string(input$platformIndex)), values$oFile)
    values$oFile <- saveLines(c(paste0("geoID <- ", format_string(input$geoID)), "series_data <- load_series(geoID, dataSetIndex)"), values$oFile)
    values$downloadChunkLen <- length(values$oFile)
    
    values$expression_oFile <- values$oFile
    values$expression_downloadChunkLen <- values$downloadChunkLen
    
    #extracted_data <- NULL
    values$origData <- values$metaData
  })
  
  #observeEvent(input$load_clinical, {
  #  values$metaData <- withProgress(process_clinical(values$allData, input$platformIndex, input$download_data_filter, session))
  #})
  observe({
    input$top_level
    if (input$top_level == "Clinical data" && is.null(values$metaData) && !is.null(values$allData)) {
      values$metaData <- withProgress(process_clinical(values$allData, session))
      
      #WRITING COMMANDS TO R SCRIPT
      values$oFile <- saveLines(commentify("extract clinical data"), values$oFile)
      values$oFile <- saveLines("clinical_data <- process_clinical(series_data)", values$oFile)
      values$downloadChunkLen <- length(values$oFile)
    }
  })
  
  output$dataset <- DT::renderDT({
    if (!is.null(values$metaData)) {
      closeAlert(session, "fileError")
      datatable(values$metaData, rownames = TRUE, options = list(
        columnDefs = list(list(
        targets = "_all",
        ##Makes it so that the table will only display the first 50 chars.
        ##See https://rstudio.github.io/DT/options.html
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && typeof data === 'string' && data.length > 50 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
          "}")
      ))))
    }
    else {
      datatable(values$default_metaData, rownames = FALSE, colnames = "NO DATA")
    }
  })
  
  # summary -----------------------------------------------------------------
  
  
  #output$metaSummary <- renderText({printVarsSummary(values$metaData)})
  
  output$choose_variable_to_view <- renderUI({
    if (!is.null(values$metaData)) {
      choices <- c(0, 1:length(colnames(values$metaData)))
      names(choices) <- c("(view all)", colnames(values$metaData))
      selectInput("variable_to_view", label = "Choose a variable to view:", choices = choices)
    }
  })
  
  if (FALSE) {
  # Create the list of plot names
  plotInput <- reactive({
    if (!is.null(values$metaData)) {
      n_plot <- ncol(values$metaData)
      total_data <- lapply(1:n_plot, function(i){as.character(values$metaData[,i])})
      return(list("n_plot" = n_plot, "total_data" = total_data))
    }
  })
  }
  
  # Create divs
  output$plots <- renderUI({
    
    if (!is.null(values$metaData)) {
      plot_output_list <- lapply(1:ncol(values$metaData), function(i) {
        #if (!grepl("evalSame", colnames(values$metaData)[i])) {
        if (is_all_unique(values$metaData[,i])) {
          div(hr(), HTML(
            paste0("<b>", colnames(values$metaData)[i], "</b> consists of all unique values.")
          ), hr())
        } else if (is_all_identical(values$metaData[,i])) {
          div(hr(), HTML(
            paste0("<b>", colnames(values$metaData)[i], "</b> consists of all the same value.")
          ), hr())
        } else {
          plotname <- make.names(colnames(values$metaData)[i])
          div(withSpinner(plotlyOutput(plotname, height = 700, width = "auto"), type = 5), tertiary_button(paste0("savePlot", i), "Download plot", class = "clinical_plot"))
        }
        #}
      })
      if (!is.null(input$variable_to_view)) {
        if (as.numeric(input$variable_to_view) == 0) {
          do.call(tagList, plot_output_list)
        } else {
          #print(plot_output_list[5][[1]])
          plot_output_list[as.numeric(input$variable_to_view)][[1]]
        }
      }
    }
  })
  # Create the actual plots associated with the plot names
  observe({
    if (!is.null(values$metaData)) {
      lapply(1:ncol(values$metaData), function(i){
        if (!is_all_unique(values$metaData[,i]) && !is_all_identical(values$metaData[,i])) {
          is_all_numeric <- isAllNum(values$metaData[i])
          if (is_all_numeric) {
            values$display_barwidth_option <- TRUE
          }
          output[[ make.names(colnames(values$metaData)[i]) ]] <- renderPlotly({
            suppressWarnings(create_plot(as.character(values$metaData[,i]), input$clinical_plot_color, input$clinical_binwidths, colnames(values$metaData)[i], isAllNum(values$metaData[i])))
          })
        }
      })
      #browser()
    }
  })
  
  observeEvent(input$last_btn_clinical, {
    if (!is.null(input$last_btn_clinical)) {
      values$clinical_plot_to_save <- as.numeric(as.character(str_remove(input$last_btn_clinical, "savePlot")))
      showModal(
        modalDialog(
          sliderInput("clinical_plot_width", label = "Image width (inches):", min = 1, max = 36, value = 6),
          sliderInput("clinical_plot_height", label = "Image height (inches):", min = 1, max = 36, value = 6),
          radioButtons("clinical_plot_filetype", label = "File type:", choices = c("PDF" = "pdf", "JPG" = "jpg", "PNG" = "png")),
          downloadButton("clinical_plot_download"),
          footer = modalButton("Close")
        ))
      session$sendCustomMessage(type = "resetValue", "last_btn_clinical")
    }
  })
  
  output$clinical_plot_download <- downloadHandler(
    filename = function() {
      paste(make.names(colnames(values$metaData)[values$clinical_plot_to_save]), input$clinical_plot_filetype, sep = ".")
    },
    content = function(file) {
      plot_to_save <- create_plot_to_save(#plotInput()$total_data[[values$clinical_plot_to_save]],
        values$metaData[,values$clinical_plot_to_save], 
                                          input$clinical_plot_color, 
                                          input$clinical_binwidths, 
                                          colnames(values$metaData)[values$clinical_plot_to_save], 
                                          isAllNum(values$metaData[values$clinical_plot_to_save]))
      
      ggsave(file, plot_to_save, width = input$clinical_plot_width, height = input$clinical_plot_height, device = input$clinical_plot_filetype)
      
    }
  )
  
  # extract columns ---------------------------------------------------------
  
  output$choose_cols_to_split <- renderUI({
    #colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    colNames <- colnames(values$metaData)
    checkboxGroupInput(inputId = "cols_to_split", label = NULL, colNames)
  })
  
  observe({
    updateCheckboxGroupInput(
      session, 'cols_to_split', choices = colnames(values$metaData),
      selected = if (input$select_all_split) colnames(values$metaData)
    )
  })
  
  output$choose_cols_to_divide <- renderUI({
    #colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    colNames <- colnames(values$metaData)
    checkboxGroupInput(inputId = "colsToDivide", label = NULL, colNames)
  })
  
  observe({
    updateCheckboxGroupInput(
      session, 'colsToDivide', choices = colnames(values$metaData),
      selected = if (input$select_all_divide) colnames(values$metaData)
    )
  })
  
  
  observeEvent(input$reformat_columns, ({
    if (!is.null(values$metaData)) {
      values$lastData <- values$metaData
      before <- length(values$oFile)
      values$oFile <- saveLines(commentify("extract values from columns with delimiter"), values$oFile)
      if (input$to_split) {
        values$metaData <- withProgress(extractColNames(values$metaData,
                                                        input$split_delimiter,
                                                        input$cols_to_split), message = "Extracting column names")
        #WRITING COMMANDS TO R SCRIPT
        values$oFile <- saveLines(paste0("cols_to_split <- ", format_string(input$cols_to_split)), values$oFile)
        values$oFile <- saveLines(c(paste0("split_delimiter <- ", format_string(input$split_delimiter)), 
                                    "clinical_data <- extractColNames(clinical_data, split_delimiter, cols_to_split)"), 
                                  values$oFile)
      } else if (input$to_divide) {
        values$metaData <- withProgress(splitCombinedVars(values$metaData,
                                                          input$colsToDivide,
                                                          input$divide_delimiter), message = "Splitting combined variables")
        #WRITING COMMANDS TO R SCRIPT
        values$oFile <- saveLines(paste0("cols_to_divide <- ", format_string(input$colsToDivide)), values$oFile)
        values$oFile <- saveLines(c(paste0("divide_delimiter <- ", format_string(input$divide_delimiter)),
                                    "clinical_data <- splitCombinedVars(clinical_data, cols_to_divide, divide_delimiter)"), 
                                  values$oFile)
        
      }
      updateCheckboxInput(session, inputId = "to_split", value = FALSE)
      updateCheckboxInput(session, inputId = "to_divide", value = FALSE)
      
      values$currChunkLen <- length(values$oFile) - before
    }

  }))
  
  
  # filter columns ----------------------------------------------------------
  
  
  observe({
    updateCheckboxGroupInput(
      session, 'varsToKeep', choices = colnames(values$metaData),
      selected = if (input$select_all_columns) colnames(values$metaData)
    )
  })
  
  output$display_vars_to_keep <- renderUI({
    #colNames <- colnames(values$metaData[-which(colnames(values$metaData) == "evalSame")])
    colNames <- colnames(values$metaData)
    checkboxGroupInput(inputId = "varsToKeep", label = NULL, 
                       choices = colNames, selected = colNames)
  })
  
  observeEvent(input$clinical_evaluate_filters, ({
    if (!is.null(values$metaData)) {
      values$lastData <- values$metaData
      
      
      before <- length(values$oFile)
      values$oFile <- saveLines(commentify("exclude undesired columns"), values$oFile)
      
      if (input$filter_option == "preset_filters") {
        values$metaData <- filterUninformativeCols(values$metaData, input$download_data_filter)
        
        #WRITING COMMANDS TO R SCRIPT
        values$oFile <- saveLines(paste0("vars_to_exclude <- ", format_string(input$download_data_filter)), values$oFile)
        values$oFile <- saveLines("clinical_data <- filterUninformativeCols(clinical_data, vars_to_exclude)", 
                                  values$oFile)
      } else {
        values$metaData <- filterCols(values$metaData, input$varsToKeep)
        
        #WRITING COMMANDS TO R SCRIPT
        values$oFile <- saveLines(paste0("vars_to_keep <- ", format_string(input$varsToKeep)), values$oFile)
        values$oFile <- saveLines(c("clinical_data <- filterCols(clinical_data, vars_to_keep)"), 
                                  values$oFile)
      }
      
      
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
    values$metaData <- renameCols(values$metaData, input$colsToRename, input$rename_new_name)
    
    #WRITING COMMANDS TO R SCRIPT
    before <- length(values$oFile)
    values$oFile <- saveLines(commentify("rename column"), values$oFile)
    values$oFile <- saveLines(paste0("clinical_data <- renameCols(clinical_data, ", format_string(input$colsToRename), ", ", format_string(input$rename_new_name), ")"), values$oFile)
    values$currChunkLen <- length(values$oFile) - before
    
    values$newNames <- NULL
  }))
  
  
  # substitute vals ---------------------------------------------------------
  
  output$input_sub_range <- renderUI({
    
    if (isAllNum(values$metaData[input$colsToSub])) {
      output = tagList()
      currCol <- as.numeric(as.character(values$metaData[!is.na(values$metaData[,input$colsToSub]),input$colsToSub]))
      this_min <- min(currCol)
      this_max <- max(currCol)
      this_quantiles <- c(quantile(currCol)[2], quantile(currCol)[3])
      if (this_quantiles[1] == this_quantiles[2]) {
        this_quantiles <- c(this_min, this_max)
      }
      output[[1]] <- sliderInput(inputId = "slideInSub", label = "Please choose a range of values (inclusive)", min = this_min, max = this_max, value = this_quantiles)
      #output[[2]] <- textInput("newRangeVal", label = "Please enter a value to replace all values in the range:")
      output[[3]] <- tertiary_button("add_val_to_sub", "Add range to table")
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
  
  
  values$suggestions <- reactive({ 
    if (!is.null(values$metaData) && !is.null(input$colsToSub)) {
      unique(as.character(values$metaData[,input$colsToSub]))
    }
  })
  
  output$input_subs_table <- renderRHandsontable({
    rhandsontable(values$DFOut, width = 350, height = 100, rowHeaders = FALSE) %>% 
      hot_col(col = "To_Replace", type = "autocomplete", source = values$suggestions(), strict = FALSE) #%>%
      #hot_col(col = "New_Val", type = "autocomplete", source = values$thes_suggest_vals, strict = FALSE)
  })
  
  observeEvent(input$input_subs_table, {
    values$DFIn <- hot_to_r(input$input_subs_table)
  })
  
  observeEvent(input$add_val_to_sub, {
    if (!is.null(input$colsToSub) && input$colsToSub != "") {
      if (all(values$DFIn["To_Replace"] == "")) {
        values$DFIn["To_Replace"] <- paste("RANGE:", paste(input$slideInSub, collapse = " - "))
      } else {
        values$DFIn <- rbind(values$DFIn, c(paste("RANGE:", paste(input$slideInSub, collapse = " - ")), ""))
      }
      values$DFOut <- values$DFIn
    }
  })
  
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
      values$metaData <- withProgress(substitute_vals(values$metaData, sub_specs, input$sub_w_regex), 
                                      message = "Substituting values")
      
      #WRITING COMMANDS TO R SCRIPT
      before <- length(values$oFile)
      values$oFile <- saveLines(commentify("substitute values"), values$oFile)
      values$oFile <- saveLines(paste0("sub_specs <- list()"), values$oFile)
      #for (i in 1:length(values$tablesList)) {
        values$oFile <- saveLines(paste0("sub_specs[[", format_string(input$colsToSub), "]] <- ",
                                         "data.frame(", colnames(values$DFIn)[1], "=c(", 
                                         paste(format_string(as.character(values$DFIn[,1])), collapse = ", "), "), ",
                                         colnames(values$DFIn)[2], "=c(", 
                                         paste(format_string(as.character(values$DFIn[,2])), collapse = ", "), "))"), values$oFile)
      #}
      values$oFile <- saveLines("clinical_data <- substitute_vals(clinical_data, sub_specs)", 
                                values$oFile)
      values$currChunkLen <- length(values$oFile) - before
      
      values$tablesList <- list()
      values$DFOut <- data.frame(To_Replace = "", New_Val = "", stringsAsFactors = FALSE)
      
    #}
  })
  
  # exclude vals ------------------------------------------------------------
  
  
  output$sliderExclude <- renderUI({
    
    if (isAllNum(values$metaData[input$col_valsToExclude])) {
      output <- tagList()
      currCol <- as.numeric(as.character(values$metaData[!is.na(values$metaData[,input$col_valsToExclude]),input$col_valsToExclude]))
      output[[1]] <- radioButtons("excludeToKeep", label = "I would like to:", 
                                  choices = list("exclude the values within the range." = "exclude", 
                                                 "keep only the values within the range." = "keep"))
      output[[2]] <- sliderInput(inputId = "sliderExclude", label = "Please choose a range of values (inclusive)", min = min(currCol), max = max(currCol), value = c(quantile(currCol)[2], quantile(currCol)[3]))
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
                choices = colNames,
                selected = values$last_selected_exclude)
  })
  
  to_exclude_options <- reactive({
    if (!is.null(input$col_valsToExclude)) {
      valNames <- unique(as.character(values$metaData[,input$col_valsToExclude]))
      valNames[which(is.na(valNames))] <- "NA"
      valNames
      
    }
  })
  
  observe({
    updateCheckboxGroupInput(
      session, 'valsToExclude', choices = to_exclude_options(),
      selected = if (input$select_all_exclude) to_exclude_options()
    )
  })
  
  output$display_vals_to_exclude <- renderUI({
    if (!is.null(input$col_valsToExclude)) {
      valNames <- unique(as.character(values$metaData[,input$col_valsToExclude]))
      valNames[which(is.na(valNames))] <- "NA"
      checkboxGroupInput(inputId = "valsToExclude", 
                         label = NULL,
                         choices = to_exclude_options())
    }
  })
  
  observeEvent(input$clinical_evaluate_exclude, {
    
    if (!is.null(input$col_valsToExclude) && (!is.null(input$valsToExclude) || !is.null(input$sliderExclude))) {
      if (input$exclude_isrange && isAllNum(values$metaData[input$col_valsToExclude])) {
        to_exclude <-  paste(input$excludeToKeep, paste(input$sliderExclude, collapse = " - "), sep = ": ")
      } 
      else {
        to_exclude <- input$valsToExclude
      }
      values$lastData <- values$metaData
      values$last_selected_exclude <- input$col_valsToExclude
      values$metaData <- withProgress(excludeVars(values$metaData, input$col_valsToExclude, to_exclude), 
                                      message = "Filtering rows")
      
      #WRITING COMMANDS TO R SCRIPT
      before <- length(values$oFile)
      values$oFile <- saveLines(commentify("exclude undesired samples"), values$oFile)
      values$oFile <- saveLines(c(paste0("variable <- ", format_string(input$col_valsToExclude)),
                                  paste0("values <- ", format_string(to_exclude))), values$oFile)
      values$oFile <- saveLines("clinical_data <- excludeVars(clinical_data, variable, values)", 
                                values$oFile)
      values$currChunkLen <- length(values$oFile) - before
    }
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
      values$oFile <- saveLines(c("clinical_data <- cbind(rownames(clinical_data), clinical_data)", 
                                  "colnames(clinical_data)[1] <- ''", 
                                  paste0("file <- ", format_string(input$clinical_user_filename))), values$oFile)
      
      if (input$clinical_file_type == "csv") {
        values$oFile <- saveLines(paste0("write.csv(clinical_data, file, row.names = FALSE)"), values$oFile)
      }
      else if (input$clinical_file_type == "tsv") {
        values$oFile <- saveLines("write.table(clinical_data, file, sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)", 
                                  values$oFile)
      }
      else if (input$clinical_file_type == "JSON") {
        values$oFile <- saveLines(c("library(jsonlite)", "library(readr)", 
                                    "clinical_data %>% toJSON() %>% write_lines(file)"), 
                                  values$oFile)
      }
      else if (input$clinical_file_type == "xlsx") {
        values$oFile <- saveLines(c("library(xlsx)", "write.xlsx(clinical_data, file, row.names = FALSE, showNA = FALSE)"), 
                                  values$oFile)
      }
      
      values$currChunkLen <- values$currChunkLen + (length(values$oFile) - before)
      
      saveToRscript(values$oFile, file)
    }
  )
  
  # navigation --------------------------------------------------------------
  
  output$start_clinical_nav_ui <- renderUI({
    div(
      secondary_button('nav_choose_to_clinical_button', 'Next - Process clinical data', class = "right_align")
    )
  })
  observeEvent(input$nav_choose_to_clinical_button, {
    updateTabsetPanel(session, 'top_level', selected = 'Clinical data')
  })
  #1  
  output$nav_1_ui <- renderUI({
    div(
      tertiary_button('nav_clinical_to_choose_button', 'Back'),
      secondary_button('nav_1_to_2_button', 'Next', class = "right_align")
    )
  })
  observeEvent(input$nav_clinical_to_choose_button, {
    updateTabsetPanel(session, 'top_level', selected = "Choose dataset")
  })
  observeEvent(input$nav_1_to_2_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '2')
  })
  #2  
  output$nav_2_ui <- renderUI({
    div(
      tertiary_button('nav_2_to_1_button', 'Back'),
      secondary_button('nav_2_to_3_button', 'Next', class = "right_align")
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
    div(
      tertiary_button('nav_3_to_2_button', 'Back'),
      secondary_button('nav_3_to_4_button', 'Next', class = "right_align")
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
    div(
      tertiary_button('nav_4_to_3_button', 'Back'),
      secondary_button('nav_4_to_5_button', 'Next', class = "right_align")
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
    div(
      tertiary_button('nav_5_to_4_button', 'Back'),
      secondary_button('nav_5_to_6_button', 'Next', class = "right_align")
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
    div(
      tertiary_button('nav_6_to_5_button', 'Back'),
      secondary_button('nav_6_to_expression_button', 'Next - Process assay data', class = "right_align")
    )
  })
  observeEvent(input$nav_6_to_5_button, {
    updateTabsetPanel(session, 'clinical_side_panel', selected = '5')
  })
  observeEvent(input$nav_6_to_expression_button, {
    updateTabsetPanel(session, 'top_level', selected = 'Assay data')
  })
  #expression 1
  output$expression_nav_1_ui <- renderUI({
    div(
      tertiary_button('nav_1_to_clinical_button', 'Back'),
      secondary_button('expression_nav_1_to_2_button', 'Next', class = 'right_align')
    )
  })
  observeEvent(input$nav_1_to_clinical_button, {
    udpateTabsetPanel(session, 'clinical_side_panel', selected = '6')
  })
  observeEvent(input$expression_nav_1_to_2_button, {
    updateTabsetPanel(session, 'expression_side_panel', selected = '2')
  })
  #expression 2
  output$expression_nav_2_ui <- renderUI({
    div(
      tertiary_button('expression_nav_2_to_1_button', 'Back')
    )
  })
  observeEvent(input$expression_nav_2_to_1_button, {
    updateTabsetPanel(session, 'expression_side_panel', selected = '1')
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
  
  observeEvent(input$regex_help, {
    help_modal("www/Regular_Expressions_Documentation.md")
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
    images <- c("download_example.gif")
    image_names <- c("Demo - Load Series")
    
    create_image_grid(images, image_names)
  })
  
  observeEvent(input$clinical_r_help, {
    help_modal("www/R_Help_Documentation.md")
  })
  
  observeEvent(input$replace_id_help, {
    help_modal("www/Different_ID_Documentation.md")
  })
  
  observeEvent(input$transpose_help, {
    help_modal("www/Transpose_Documentation.md")
  })
  
  observeEvent(input$filter_help, {
    help_modal("www/Filter_Data_Documentation.md")
  })
  
  observeEvent(input$evaluate_filters_help, {
    help_modal("www/Apply_Filters_Documentation.md")
  })
  
  observeEvent(input$expression_r_help, {
    help_modal("www/R_Help_Documentation.md")
  })
  
  observeEvent(input$clinical_files_help, {
    help_modal("www/File_Types_Documentation.md")
  })
  
  observeEvent(input$expression_files_help, {
    help_modal("www/File_Types_Documentation.md")
  })
  
  observeEvent(input$clickimg, {
    showModal(modalDialog({
      tags$img(src = input$clickimg, width = "100%", height = "100%")
    },
    size = "l"
    ))
  }, ignoreInit = TRUE)
  

  # expression data sidebar -------------------------------------------------
   
  observeEvent(input$top_level, {
    input$top_level
    if (!is.null(values$allData) && input$top_level == "Assay data" && is.null(values$exprData)) {
      extracted_data <- withProgress(process_expression(values$allData, input$platformIndex, session))
      
      values$expression_warning_state <- extracted_data[["status"]]
      
      values$orig_expr <- extracted_data[["expressionData"]]
      values$last_expr <- values$orig_expr
      values$expr_data <- values$orig_expr
      if (is.null(values$expr_data)) {
        values$default_expr_data <- data.frame(paste0("No assay data available for ", input$geoID))
        values$default_ft_data <- data.frame(paste0("No feature data available for ", input$geoID))
      } else {
        values$expr_to_display <- advance_columns_view(values$expr_data, 
                                                       start = 1, 
                                                       forward_distance = 5, 
                                                       previous_view = values$expr_data)
        values$orig_feature <- find_intersection(extracted_data[["featureData"]], values$expr_to_display)
        values$last_feature <- values$orig_feature
        values$feature_data <- values$orig_feature
        values$feature_to_display <- advance_columns_view(values$feature_data, 
                                                          start = 1, 
                                                          forward_distance = 4, 
                                                          previous_view = values$feature_data)
        
        values$expression_oFile <- saveLines(commentify("download expression data"), values$expression_oFile)
        values$expression_oFile <- saveLines(paste0("dataSetIndex <- ", format_string(input$platformIndex)), values$expression_oFile)
        values$expression_oFile <- saveLines(c(paste0("geoID <- ", format_string(input$geoID)),
                                               "allData <- downloadExpression(geoID, dataSetIndex)",
                                               "expressionData <- allData[['expressionData']]",
                                               "featureData <- allData[['featureData']]"), 
                                             values$expression_oFile)
        
        values$expression_downloadChunkLen <- length(values$expression_oFile)
      }
      rm(extracted_data)
    }
  })
  
  # feature data modal -------------------------------------------------
  
  output$featureData <- DT::renderDT({
    if (!is.null(values$feature_to_display)) {
      datatable(values$feature_to_display, filter = "top", rownames = FALSE, options = list(dom = "tp", 
                                                                                            pageLength = 5,
                                                                                            columnDefs = list(list(
                                                                                              targets = "_all",
                                                                                              ##Makes it so that the table will only display the first 30 chars.
                                                                                              ##See https://rstudio.github.io/DT/options.html
                                                                                              render = JS(
                                                                                                "function(data, type, row, meta) {",
                                                                                                "return type === 'display' && typeof data === 'string' && data.length > 15 ?",
                                                                                                "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                                                                                                "}")
                                                                                            ))))
    }
    else {
      datatable(values$default_ft_data, rownames = FALSE, 
                colnames = "NO DATA", options = list(dom = "tp"))
    }
  }) 
  
  observeEvent(input$expression_replace_id, {
    showModal(
      modalDialog(
        fluidRow(
          column(1, secondary_button(id = "feature_prev_cols", label = div(icon("arrow-left"), "Previous columns"))),
          column(1, offset = 8, secondary_button(id = "feature_next_cols", label = div("Next columns", icon("arrow-right"))))
        ),
        withSpinner(dataTableOutput("featureData"), type = 5),
        uiOutput("exprLabels"),
        uiOutput("summarizeOptions"),
        
        footer = primary_button(id = "expression_evaluate_id", label = "Replace ID column"), 
        title = "Feature data",
        size = "l",
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$feature_next_cols, {
    if (!is.null(values$feature_data)) {
      values$feature_to_display <- advance_columns_view(values$feature_data, 
                                                        start = colnames(values$feature_to_display)[ncol(values$feature_to_display)], 
                                                        forward_distance = 4, 
                                                        previous_view = values$feature_to_display)
    }
  })
  
  observeEvent(input$feature_prev_cols, {
    if (!is.null(values$feature_data)) {
      values$feature_to_display <- retract_columns_view(values$feature_data, 
                                                        last_column = colnames(values$feature_to_display)[2], 
                                                        backward_distance = 4, 
                                                        previous_view = values$feature_to_display)
    }
  })
  
  observeEvent(input$expression_evaluate_id, {
    
    removeModal()
    
    if (!is.null(values$expr_data)) {
      values$last_expr <- values$expr_data
      
      feature_data <- values$feature_data
      
      if (values$feature_id_col != "ID") {
        colnames(feature_data)[which(colnames(feature_data) == "ID")] <- 
          colnames(values$orig_feature[which(colnames(feature_data) == "ID")])
        colnames(feature_data)[which(colnames(feature_data) == values$feature_id_col)] <- "ID"
        if (length(which(colnames(feature_data) == "ID")) > 1) {
          feature_data <- feature_data[,-1]
        }
      }
      
      values$expr_data <- withProgress(message = "Replacing the ID column", 
                                       replaceID(values$expr_data, feature_data, input$colForExprLabels, input$howToSummarize))
      values$feature_id_col <- input$colForExprLabels
      
      before <- length(values$expression_oFile)
      
      #WRITING COMMANDS TO EXPRESSION RSCRIPT
      values$expression_oFile <- saveLines(c(commentify("replace ID column"),
                                             paste0("expressionData <- replaceID(expressionData, featureData, ", 
                                                    format_string(input$colForExprLabels), ", ",
                                                    format_string(input$howToSummarize), ")")), 
                                           values$expression_oFile)
      
      values$expr_to_display <- advance_columns_view(values$expr_data, 
                                                     start = 1, 
                                                     forward_distance = 5, 
                                                     values$expr_data)
      
      after <- length(values$expression_oFile)
      
      values$expression_currChunkLen <- after - before
    }
    
    #shinyjs::disable("expression_replace_id")
    
  })
  
  output$exprLabels <- renderUI({
    selectInput("colForExprLabels", label = div("Please select a column to replace the expression IDs", 
                                                help_button("To keep the same ID column, please choose ID.")), 
                choices = colnames(values$feature_data)[which(!colnames(values$feature_data) == "ID")]
                )
  })
  
  output$summarizeOptions <- renderUI({
    can_summarize <- !is.null(input$colForExprLabels) && input$colForExprLabels != "" && !is_all_unique(values$feature_data[, input$colForExprLabels]) #!input$colForExprLabels %in% findExprLabelColumns(values$feature_data)
    if (can_summarize) {
      choices <- if (values$expression_warning_state) c("mean", "median", "max", "min", "keep all") else c("keep all")
      selectInput("howToSummarize", label = div("It looks like this column contains multiple values for one expression ID.
                How would you like to summarize the data?", help_button("Groups the data by ID and takes the specified measurement for the group.")), 
                  choices = choices)
    }
  })
  
  observe({
    shinyjs::toggleState("expression_replace_id", condition = !values$expression_disable_btns)
  })
  
  observe({
    disable_transpose <- !values$expression_disable_btns & length(unique(values$expr_data$ID)) == nrow(values$expr_data)
    shinyjs::toggleState("expression_transpose", disable_transpose)
  })

  # other expression options ------------------------------------------------

  observeEvent(input$expression_transpose, {
    if (!is.null(values$expr_data)) {
      values$last_expr <- values$expr_data
      
      before <- length(values$expression_oFile)
      
      values$expr_data <- withProgress(message = "Transposing the data", 
                                       quickTranspose(values$expr_data))
      
      values$expression_id_col <- "colnames"
      
      #WRITING COMMANDS TO EXPRESSION RSCRIPT
      values$expression_oFile <- saveLines(c(commentify("transpose data"), 
                                             "expressionData <- quickTranspose(expressionData)"), 
                                           values$expression_oFile)
      
      values$expr_to_display <- advance_columns_view(values$expr_data, 
                                                     start = 1, 
                                                     forward_distance = 5, 
                                                     previous_view = values$expr_data)
      
      after <- length(values$expression_oFile)
      
      values$expression_currChunkLen <- after - before
      values$expression_disable_btns <- TRUE
    }
  })
  
  
  
  observeEvent(input$expression_evaluate_filters, {
    #TODO: debug filtering when the data is transposed
    values$last_expr <- values$expr_data
    values$last_feature <- values$feature_data
    
    to_filter <- input$exprPreview_search_columns
    names(to_filter) <- colnames(values$expr_to_display)
    
    values$expr_data <- filterExpressionData(values$expr_data, to_filter)
    values$expr_to_display <- advance_columns_view(values$expr_data, 
                                                   start = 1, 
                                                   forward_distance = 5, 
                                                   previous_view = values$expr_data)
    values$feature_data <- find_intersection(values$feature_data, values$expr_data, values$feature_id_col, values$expression_id_col)
    values$feature_to_display <- advance_columns_view(values$feature_data, 
                                                      start = 1, 
                                                      forward_distance = 4, 
                                                      previous_view = values$feature_data)
    
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
  
  observeEvent(input$undoEvalExpr, {
    if (!is.null(values$expr_data)) {
      
      #if replaceID is in the last chunk, then enable the button again
      file_len <- length(values$expression_oFile)
      if (any(grepl("quickTranspose", values$expression_oFile[(file_len - (values$expression_currChunkLen - 1)):file_len]))) {
        values$expression_disable_btns <- FALSE
      }
      
      values$feature_data <- values$last_feature
      values$feature_to_display <- advance_columns_view(values$feature_data, 
                                                        start = 1, 
                                                        forward_distance = 4, 
                                                        previous_view = values$feature_data)
      values$expr_data <- values$last_expr
      values$expr_to_display <- advance_columns_view(values$expr_data, 
                                                     start = 1, 
                                                     forward_distance = 5, 
                                                     previous_view = values$expr_data)
      values$expression_oFile <- removeFromScript(values$expression_oFile, len = values$expression_currChunkLen)
      values$expression_currChunkLen <- 0
    }
  })
  
  observeEvent(input$resetExpr, {
    if (!is.null(values$expr_data)) {
      values$expression_disable_btns <- FALSE
      values$feature_data <- values$orig_feature
      values$feature_to_display <- advance_columns_view(values$feature_data, 
                                                        start = 1, 
                                                        forward_distance = 4, 
                                                        previous_view = values$feature_data)
      values$expr_data <- values$orig_expr
      values$expr_to_display <- advance_columns_view(values$expr_data, 
                                                     start = 1, 
                                                     forward_distance = 5, 
                                                     previous_view = values$expr_data)
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
          write.csv(values$expr_data, file, row.names = FALSE)
          incProgress()
        })
      }
      else if (input$expression_fileType == "tsv") {
        withProgress(message = "Writing data to file",
                     write.table(values$expr_data, file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE))
      }
      else if (input$expression_fileType == "JSON") {
        library(jsonlite)
        #library(readr)
        
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
      
      saveToRscript(values$expression_oFile, file)
      
      values$expression_currChunkLen <- values$expression_currChunkLen + (length(values$expression_oFile) - before)
    }
  )
  
  # main panel expression data ----------------------------------------------
  
  observeEvent(input$expression_next_cols, {
    if (!is.null(values$expr_data)) {
      values$expr_to_display <- advance_columns_view(values$expr_data, 
                                                     start = colnames(values$expr_to_display)[ncol(values$expr_to_display)], 
                                                     forward_distance = 5, 
                                                     previous_view = values$expr_to_display)
    }
  })
  
  observeEvent(input$expression_prev_cols, {
    if (!is.null(values$expr_data)) {
      values$expr_to_display <- retract_columns_view(values$expr_data, 
                                                     last_column = colnames(values$expr_to_display)[2], 
                                                     backward_distance = 5, 
                                                     previous_view = values$expr_to_display)
    }
  })
  
  output$exprPreview <- DT::renderDT({
    if (!is.null(values$expr_to_display)) {
      datatable(values$expr_to_display, filter = "top", rownames = FALSE, options = list(dom = "tp"))
    } else {
      datatable(values$default_expr_data, rownames = FALSE, 
                colnames = "NO DATA", options = list(dom = "tp"))
    }
  })

  # graphical summary -------------------------------------------------------
  
  output$expr_select_binwidths <- renderUI({
    if (is.null(values$expr_to_display) || !values$expression_warning_state) {
      upper_lim <- 10
    } else {
      upper_lim <- ceiling(max(values$expr_to_display[,which(colnames(values$expr_to_display) != "ID")], na.rm = TRUE))
    }
    sliderInput("expression_binwidths", "Width of bars (for numeric):", min = 0, max = upper_lim, value = ceiling(upper_lim / 2))
  })
  
  # Create divs
  output$histograms_expression <- renderUI({
    
    if (!is.null(values$expr_to_display)) {
      plot_output_list <- lapply(2:ncol(values$expr_to_display), function(i) {
        plotname <- make.names(colnames(values$expr_to_display)[i])
        div(withSpinner(plotlyOutput(plotname, height = 500, width = "auto"), type = 5), tertiary_button(paste0("savePlot", i), "Download plot", class = "expression_plot"))
      })   
      do.call(tagList, plot_output_list)
    }
  })
  # Create the actual plots associated with the plot names
  observe({
    if (!is.null(values$expr_to_display)) {
      lapply(2:ncol(values$expr_to_display), function(i){
        output[[ make.names(colnames(values$expr_to_display)[i]) ]] <- renderPlotly({
          suppressWarnings(create_plot(as.character(values$expr_to_display[,i]), input$expr_plot_color, input$expression_binwidths, colnames(values$expr_to_display)[i], is_numeric = isAllNum(values$expr_to_display[i])))
        })
      })
    }
  })
  
  observeEvent(input$last_btn_expression, {
    values$expr_plot_to_save <- as.numeric(as.character(str_remove(input$last_btn_expression, "savePlot")))
    showModal(
      modalDialog(
        sliderInput("expr_plot_width", label = "Image width (inches):", min = 1, max = 36, value = 6),
        sliderInput("expr_plot_height", label = "Image height (inches):", min = 1, max = 36, value = 6),
        radioButtons("expr_plot_filetype", label = "File type:", choices = c("PDF" = "pdf", "JPG" = "jpg", "PNG" = "png")),
        downloadButton("expr_plot_download"),
        footer = modalButton("Close")
      ))
  })
  
  output$expr_plot_download <- downloadHandler(
    filename = function() {
      paste(make.names(colnames(values$expr_to_display)[values$expr_plot_to_save]), input$expr_plot_filetype, sep = ".")
    },
    content = function(file) {
      
      plot_to_save <- create_plot_to_save(as.character(values$expr_to_display[,values$expr_plot_to_save]), 
                                  input$expr_plot_color, 
                                  input$expression_binwidths, 
                                  colnames(values$expr_to_display)[values$expr_plot_to_save], 
                                  is_numeric = isAllNum(values$expr_to_display[values$expr_plot_to_save]))
      
      ggsave(file, plot_to_save, width = input$expr_plot_width, height = input$expr_plot_height, device = input$expr_plot_filetype)
      
    }
  )
  
}

shinyApp(ui = ui, server = server)
