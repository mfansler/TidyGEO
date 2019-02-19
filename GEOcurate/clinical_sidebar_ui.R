sidebarPanel(
  useShinyjs(),
  tabsetPanel(id = "clinical_side_panel",
              
              
              # download data -----------------------------------------------------------
              
              
              tabPanel("1",
                       h4("Importing the data"),
                       div("Welcome to GEOcurate! This application will allow you to reformat data
                           from ",
                           a(target = "_blank", href = "https://www.ncbi.nlm.nih.gov/geo/", "Gene Expression Omnibus,"),
                           " which can then be used to answer research questions. To get started,",
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
                       conditionalPanel(condition = "input.to_split == true",
                                        uiOutput("choose_cols_to_split"),
                                        checkboxInput(inputId = "split_all_but", label = tags$i("Split all BUT the specified")),
                                        textInput(inputId = "split_delimiter", label = "Delimiter (including any spaces): ")
                       ),
                       checkboxInput(inputId = "to_divide", label = div("Choose columns with multiple values in one column",
                                                                        help_link(id = "divide_help"))),
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
                       div(tags$b("Which columns would you like to keep?"),
                           help_button("This will drop any unselected columns from the dataset.")),
                       checkboxInput(inputId = "keep_all_but", label = tags$i("Keep all BUT the specified")),
                       uiOutput("display_vars_to_keep"),
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
                       textInput(inputId = "rename_new_name", label = "Please specify a new name for the column."),
                       primary_button(id = "rename", label = "Rename column"),
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
                       h5('Click "Add" to add rows or "Remove" to remove the last row.'),
                       rHandsontableOutput("input_subs_table"),
                       conditionalPanel(condition = "input.substitute_isrange == false",
                                        checkboxInput("sub_w_regex", div("Use regex",
                                                                         help_button("What is regex?"))))
                       ,
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
                         column(7, offset = 3, div(downloadButton("clinical_save_rscript", "R script", 
                                                                  style = "color: #fff; background-color: #62c18b; border-color: #62c18b"),
                                                   help_link(id = "clinical_r_help")))
                       ),
                       hr(), uiOutput("nav_7_ui")
                       )
              )
)