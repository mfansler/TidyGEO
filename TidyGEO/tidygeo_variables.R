
# Libraries ---------------------------------------------------------------

# Set this to true if running on a new machine, outside of a Docker container
install_if_inexistent <- FALSE

# The libraries used in the app
suppressPackageStartupMessages({
  # D
  library(dplyr)
  library(DT)
  
  # F
  library(feather)
  
  # G
  library(GEOquery)
  library(ggplot2)
  
  # J
  library(janitor)
  
  # P
  library(plotly)
  
  # R
  library(RColorBrewer)
  library(readr)
  library(rhandsontable)
  library(rlang)
  library(rmarkdown)
  
  # S
  library(shiny)
  library(shinyBS)
  library(shinycssloaders)
  library(shinydashboard)
  library(shinyjs)
  library(shinyWidgets)
  library(stringr)
  
  # T
  library(tidyr)
  library(tidyverse)
})

libs <- c(
  # D
  "dplyr",
  "DT",
  
  # F
  "feather",
  
  # G
  "GEOquery",
  "ggplot2",
  
  # P
  "plotly",
  
  # R
  "RColorBrewer",
  "readr",
  "rhandsontable",
  "rlang",
  "rmarkdown",
  
  # S
  "shiny",
  "shinyBS",
  "shinycssloaders",
  "shinydashboard",
  "shinyjs",
  "shinyWidgets",
  "stringr",
  
  # T
  "tidyr",
  "tidyverse"
)

# An expression that will attach GEOquery if it is installed, otherwise it will install and attach
load_geoquery_if_exists <- rlang::expr(
  if (!suppressWarnings(require(lib_name, quietly = TRUE, character.only = TRUE))) {
    source("https://bioconductor.org/biocLite.R")
    BiocInstaller::biocLite("GEOquery")
    library(GEOquery)
  }
)

# An expression that will attach a package if it is installed, otherwise it will install and attach
load_library_if_exists <- rlang::expr(
  if (!suppressWarnings(require(lib_name, quietly = TRUE, character.only = TRUE))) {
    install.packages(lib_name)
    library(lib_name, character.only = TRUE)
  }
)

# Load all the libraries listed above
# suppressPackageStartupMessages({
#   lapply(libs, function(lib_name) {
#     if (install_if_inexistent) {
#       if (lib_name == "GEOquery")
#         eval(load_geoquery_if_exists)
#       else
#         eval(load_library_if_exists)
#     } else {
#       library(lib_name, character.only = TRUE)
#     }
#   })
# })


# Global variables --------------------------------------------------------

# The version number of this build
VERSION <- suppressWarnings(readLines("VERSION"))

# Icon used to represent combined data
ALL_ICON <- icon("cubes")

# The datatype names that are allowed (used to reference the different DataType variables)
ALLOWED_DATATYPES <- c("clinical", "assay", "feature", "all")
# The error message that prints when the programmer tries to reference a DataType not in the list
INVALID_DATATYPE_MESSAGE <- paste0('Please specify a valid data type (', 
                                   paste(ALLOWED_DATATYPES[-length(ALLOWED_DATATYPES)], collapse = ", "), 
                                   ", or ", ALLOWED_DATATYPES[length(ALLOWED_DATATYPES)], ")")

# The sections of the rscript that the programmer can write to
ALLOWED_SECTIONS <- c("body", "end")
# The error message that prints when the programmer tries to reference a section of the
# rscript not in the list
INVALID_SECTION_MESSAGE <- paste0('Please specify a valid section (', 
                                  paste(ALLOWED_SECTIONS[-length(ALLOWED_SECTIONS)], collapse = ", "), 
                                  " or ", ALLOWED_SECTIONS[length(ALLOWED_SECTIONS)], ")")

# Icon used to represent assay data
ASSAY_ICON <- icon("microscope")

# A message to the user when the assay data is not numeric
ASSAY_NONNUMERIC_MESSAGE <- paste("Some of the data is non-numeric.",
                                  'This data cannot be summarized in the "Use different ID column" section.',
                                  "For more data analysis options, feel free to download the data to edit with another application.")

# The font size for the graphs
BASE_SIZE <- 12

# A template for the barplots in the graphical summary
BASE_BARPLOT <- ggplot() +
  labs(x = "Values",
                y = "Count") +
  theme_bw(base_size = BASE_SIZE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                 plot.title = element_text(hjust = 0.5))

BASE_BARPLOT_TO_SAVE <- expr(
  ggplot() +
    labs(x = "Values",
         y = "Count") +
    theme_bw(base_size = BASE_SIZE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          plot.title = element_text(hjust = 0.5))
)

# A template for the histograms in the graphical summary
BASE_HISTOGRAM <- ggplot() +
  labs(x = "Values",
                y = "Frequency") +
  theme_bw(base_size = BASE_SIZE) +
  theme(plot.title = element_text(hjust = 0.5))

BASE_HISTOGRAM_TO_SAVE <- expr(
  ggplot() +
    labs(x = "Values",
         y = "Frequency") +
    theme_bw(base_size = base_size) +
    theme(plot.title = element_text(hjust = 0.5))
)

# Formatting specifications passed to the "options" parameter of most tables in the app
BASIC_TABLE_OPTIONS <- list(
  dom = "ltip",
  scrollX = TRUE,
  stateSave = TRUE,
  columnDefs = list(
    list(
      targets = "_all",
      ##Makes it so that the table will only display the first 50 chars.
      ####See https://rstudio.github.io/DT/options.html
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && typeof data === 'string' && data.length > 50 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
        "}"
      )
    )
  )
)

# Icon that represents user choice
CHOOSE_DATASET_ICON = icon("calendar")

# Icon used to represent clinical data
CLINICAL_ICON <- icon("notes-medical")

# The length of a section header (in the format "# [] ------") that will be written to the R script
COMMENT_LENGTH <- 75

# The default number of pages to view in a table
DEFAULT_PAGELEN <- 10

# An icon used to represent excluding (filtering) values from a column
EXCLUDE_ICON <- icon("filter")

# Icon used to represent feature data
FEATURE_ICON <- icon("dna")

# An icon used to represent filtering features
FILTER_ICON <- icon("square-minus")

# A list of all the formatting helper functions, their library dependencies, and their
# function dependencies, for use with R script writing
FUNC_LISTS <- readRDS("rscript_functions.rds")

# An icon for help buttons
HELP_ICON <- icon("circle-question")

# A regex to find invalid characters for column names. 
# These correspond to the characters not allowed in BRB array tools
# files. See the BRB array tools documentation for more information.
INVALID_NAME_CHARS <- "[\\\\\\/:\\*\\?\\<\\>\\=\\+\\#\\~\\`\\'\\;\\&\\%\\$\\@\\!]"

# A replacement character for the invalid characters above
VALID_REPLACEMENT <- "_"

# An icon for links which lead outside the application
LINK_OUT_ICON <- icon("arrow-up-right-from-square")

# The maximum number of bins a user can see in a graph
MAX_BINS <- 80

# How many columns to advance when the user clicks "next columns" or "previous columns"
MOVE_BY <- 5

# An icon for buttons which navigate to the next item
NEXT_ICON <- icon("arrow-right")

# The list of platforms associated with SERIES_LIST; may not be a complete list
# if the app hasn't been updated in a while
PLATFORM_LIST <- read_feather("www/platform_list.feather")

# The string corresponding to the colorbrewer palatte used for the graphs
PLOT_COLORS <- "RdYlBu"

# The units for the plot size
PLOT_UNITS <- "cm"

# An icon for buttons which navigate to the previous item
PREV_ICON <- icon("arrow-left")

# An icon used to represent replacing a column from one DataType with a column
# from another
REPLACE_ID_ICON <- icon("right-left")

# An icon used to represent renaming something
RENAME_ICON <- icon("pencil")

# An icon for reset buttons
RESET_ICON <- icon("backward-step")

# An icon used to represent downloading data from the app to a personal machine
SAVE_ICON <- icon("download")

# The list of series available to load from GEO; may not be a complete list
# if the app hasn't been updated in a while
SERIES_LIST <- read_feather("www/series_list.feather")

# An icon used to represent shifting values from one column to another
SHIFT_ICON <- icon("angles-right")

# The type of spinner to display for items with a loading spinner
SPINNER_TYPE <- 5

# An icon used to represent splitting a column that contains multiple variables
SPLIT_COLS_ICON <- icon("table-list")

# An icon used to represent splitting pairs of items
#SPLIT_PAIRS_ICON <- icon("align-justify", lib="glyphicon")
SPLIT_PAIRS_ICON <- icon("map")

# The message at the beginning of the outputted R script
START_MESSAGE <- c(paste("# Script generated using version", VERSION, 
                         "of TidyGEO (https://tidygeo.shinyapps.io/tidygeo/), an"),
                   "# application that allows scientists to quickly download and reformat data from",
                   "# the online repository Gene Expression Omnibus (GEO).",
                   "",
                   "")

# An icon used to represent substituting one value for another
SUBSTITUTE_ICON <- icon("right-left")

# The string that will be returned when a function is evaluated successfully
SUCCESS <- "completed"

# File types that can be downloaded using the app
SUPPORTED_FILE_TYPES <- c("Comma-separated file" = "csv", 
                          "Tab-separated file" = "tsv", 
                          "JSON" = "json", 
                          "Excel" = "xlsx", 
                          "Formatted for BRB Array Tools" = "txt")

# A message above the tabset for all the operations you can perform for each datatype
TABS_MESSAGE <- div(icon("arrow-left"), " tools (visit in any order) ", icon("arrow-right"), align = "center")

# An icon used to represent transposing a data frame
TRANSPOSE_ICON <- icon("retweet")

# An icon for undo buttons
UNDO_ICON <- icon("backward", lib="glyphicon")



# Naming schema -----------------------------------------------------------

# Hoping to save you a bit of refactoring if you happen to want to change any of these variable names.
# Please note that changing the words "clinical", "assay", "feature", or "all", or the names
# of the following variables/functions will still cause need for extensive refactoring.

# The separator used to generate variable names
SEP <- "_"

# The name of the variable which holds the series information in the R script
SERIES <- "series_data"

# The name of the variable which stores the main dataframe within the main reactiveVariables list
DATA <- expr(paste(datatype, "data", sep = SEP))
dataname <- function(datatype) eval(DATA)

# The name of the variable of type DataType which is a reactiveVariables list
VARIABLE <- expr(paste(datatype, "vals", sep = SEP))
varname <- function(datatype) eval(VARIABLE)

# The variable to view in a graphical summary
VAR_TO_VIEW <- expr(
  if (is.null(extra_tag))
    paste("variable", "to", "view", datatype, extra_tag, sep = SEP)
  else
    paste("variable", "to", "view", datatype, extra_tag, sep = SEP)
)
var_to_view <- function(datatype, extra_tag = NULL) eval(VAR_TO_VIEW)

# The name of the reactive variable which stores the current viewing subset of the data in a DataType
# object
VIEW <- expr(paste(datatype, "in", "view", sep = SEP))
view <- function(datatype) eval(VIEW)

# The name of a table which displays the data in a DataType
DISPLAY <- expr(
  if (is.null(extra_tag))
    paste(datatype, "display", sep = SEP)
  else 
    paste(datatype, extra_tag, "display", sep = SEP)
)
display <- function(datatype, extra_tag = NULL) eval(DISPLAY)

# The name of a textOutput which displays a message with how many columns are visible for the
# current viewing subset of a DataType
VISIBLE <- expr(
  if (is.null(extra_tag))
    paste("cols_visible", datatype, sep = SEP)
  else
    paste("cols_visible", datatype, extra_tag, sep = SEP)
)
visible <- function(datatype, extra_tag = NULL) eval(VISIBLE)

# The name of the input button which signals a change of tabs
NAV <- expr(
  if (is.null(section)) 
    paste("nav", from, "to", prev, sep = SEP) 
  else 
    paste("nav", from, "to", prev, section, sep = SEP)
)
nav <- function(from, prev, section = NULL) eval(NAV)

# Names of input buttons which signal a change in the viewing subset of data in DataType
PREV_COL_LAB <- "prev_cols"
PREV_COL <- expr(
  if (is.null(extra_tag))
    paste(PREV_COL_LAB, datatype, sep = "_")
  else
    paste(PREV_COL_LAB, datatype, extra_tag, sep = "_")
)
prev_col <- function(datatype, extra_tag = NULL) eval(PREV_COL)
prev_col_source <- function(input_id) str_remove(input_id, paste0(PREV_COL_LAB, SEP))

NEXT_COL_LAB <- "next_cols"
NEXT_COL <- expr(
  if (is.null(extra_tag))
    paste("next_cols", datatype, sep = "_")
  else
    paste("next_cols", datatype, extra_tag, sep = "_")
)
next_col <- function(datatype, extra_tag = NULL) eval(NEXT_COL)
next_col_source <- function(input_id) str_remove(input_id, paste0(NEXT_COL_LAB, SEP))
