
# Global variables --------------------------------------------------------

# The version number of this build
VERSION <- suppressWarnings(readLines("VERSION"))

# Icon used to represent combined data
ALL_ICON <- shiny::icon("cubes")

# The datatype names that are allowed (used to reference the different DataType variables)
ALLOWED_DATATYPES <- c("clinical", "assay", "feature", "all")
# The error message that prints when the programmer tries to reference a DataType not in the list
INVALID_DATATYPE_MESSAGE <- paste0('Please specify a valid data type (', 
                                   paste(allowed_datatypes[-length(allowed_datatypes)], collapse = ", "), 
                                   ", or ", allowed_datatypes[length(allowed_datatypes)], ")")

# The sections of the rscript that the programmer can write to
ALLOWED_SECTIONS <- c("body", "end")
# The error message that prints when the programmer tries to reference a section of the
# rscript not in the list
INVALID_SECTION_MESSAGE <- paste0('Please specify a valid section (', 
                                  paste(allowed_sections[-length(allowed_sections)], collapse = ", "), 
                                  " or ", allowed_sections[length(allowed_sections)], ")")

# Icon used to represent assay data
ASSAY_ICON <- shiny::icon("microscope")

# The font size for the graphs
BASE_SIZE <- 18

# A template for the barplots in the graphical summary
BASE_BARPLOT <- ggplot() +
  labs(x = "Values",
       y = "Count") +
  theme_bw(base_size = BASE_SIZE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))

# A template for the histograms in the graphical summary
BASE_HISTOGRAM <- ggplot() +
  labs(x = "Values",
       y = "Frequency") +
  theme_bw(base_size = BASE_SIZE) +
  theme(plot.title = element_text(hjust = 0.5))

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

# Icon used to represent clinical data
CLINICAL_ICON <- shiny::icon("clipboard")

# The length of a section header (in the format "# [] ------") that will be written to the R script
COMMENT_LENGTH <- 75

# The default number of pages to view in a table
DEFAULT_PAGELEN <- 10

# Icon used to represent feature data
FEATURE_ICON <- shiny::icon("dna")

# A list of all the formatting helper functions, their library dependencies, and their
# function dependencies, for use with R script writing
FUNC_LISTS <- readRDS("User/rscript_functions.rds")

# An icon for help buttons
HELP_ICON <- shiny::icon("question-circle")

# An icon for links which lead outside the application
LINK_OUT_ICON <- shiny::icon("external-link")

# How many columns to advance when the user clicks "next columns" or "previous columns"
MOVE_BY <- 5

# An icon for buttons which navigate to the next item
NEXT_ICON <- shiny::icon("arrow-right")

# The list of platforms associated with SERIES_LIST; may not be a complete list
# if the app hasn't been updated in a while
PLATFORM_LIST <- read_feather("www/platform_list.feather")

# An icon for buttons which navigate to the previous item
PREV_ICON <- shiny::icon("arrow-left")

# The list of series available to load from GEO; may not be a complete list
# if the app hasn't been updated in a while
SERIES_LIST <- read_feather("www/series_list.feather")

# The message at the beginning of the outputted R script
START_MESSAGE <- c(paste("# Script generated using version", VERSION, 
                         "of TidyGEO (https://tidygeo.shinyapps.io/tidygeo/), an"),
                   "# application that allows scientists to quickly download and reformat data from",
                   "# the online repository Gene rlang::expression Omnibus (GEO).",
                   "",
                   "")

# The string that will be returned when a function is evaluated successfully
SUCCESS <- "completed"

# An icon for reset buttons
RESET_ICON <- shiny::icon("history")

# An icon for undo buttons
UNDO_ICON <- shiny::icon("undo")


# Naming schema -----------------------------------------------------------

# Hoping to save you a bit of refactoring if you happen to want to change any of these variable names.
# Please note that changing the words "clinical", "assay", "feature", or "all", or the names
# of the following variables/functions will still cause need for extensive refactoring.

# The separator used to generate variable names
SEP <- "_"

# The name of the variable which holds the series information in the R script
SERIES <- "series_data"

# The name of the variable which stores the main dataframe within the main reactiveVariables list
DATA <- rlang::expr(paste(datatype, "data", sep = SEP))
dataname <- function(datatype) eval(DATA)

# The name of the variable of type DataType which is a reactiveVariables list
VARIABLE <- rlang::expr(paste(datatype, "vals", sep = SEP))
varname <- function(datatype) eval(VARIABLE)

# The name of the reactive variable which stores the current viewing subset of the data in a DataType
# object
VIEW <- rlang::expr(paste(datatype, "in", "view", sep = SEP))
view <- function(datatype) eval(VIEW)

# The name of a table which displays the data in a DataType
DISPLAY <- rlang::expr(
  if (is.null(extra_tag))
    paste(datatype, "display", sep = SEP)
  else 
    paste(datatype, extra_tag, "display", sep = SEP)
)
display <- function(datatype, extra_tag = NULL) eval(DISPLAY)

# The name of a textOutput which displays a message with how many columns are visible for the
# current viewing subset of a DataType
VISIBLE <- rlang::expr(
  if (is.null(extra_tag))
    paste("cols_visible", datatype, sep = SEP)
  else
    paste("cols_visible", datatype, extra_tag, sep = SEP)
)
visible <- function(datatype, extra_tag = NULL) eval(VISIBLE)

# The name of the input button which signals a change of tabs
NAV <- rlang::expr(
  if (is.null(section)) 
    paste("nav", from, "to", prev, sep = SEP) 
  else 
    paste("nav", from, "to", prev, section, sep = SEP)
)
nav <- function(from, prev, section = NULL) eval(NAV)

# Names of input buttons which signal a change in the viewing subset of data in DataType
PREV_COL_LAB <- "prev_cols"
PREV_COL <- rlang::expr(
  if (is.null(extra_tag))
    paste(PREV_COL_LAB, datatype, sep = "_")
  else
    paste(PREV_COL_LAB, datatype, extra_tag, sep = "_")
)
prev_col <- function(datatype, extra_tag = NULL) eval(PREV_COL)
prev_col_source <- function(input_id) stringr::str_remove(input_id, paste0(PREV_COL_LAB, SEP))

NEXT_COL_LAB <- "next_cols"
NEXT_COL <- rlang::expr(
  if (is.null(extra_tag))
    paste("next_cols", datatype, sep = "_")
  else
    paste("next_cols", datatype, extra_tag, sep = "_")
)
next_col <- function(datatype, extra_tag = NULL) eval(NEXT_COL)
next_col_source <- function(input_id) stringr::str_remove(input_id, paste0(NEXT_COL_LAB, SEP))