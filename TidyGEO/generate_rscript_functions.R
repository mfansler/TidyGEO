library(stringr)
library(readr)

# Read the file with the formatting functions
func_strings <- read_lines("~/R_Code/TidyGEO/TidyGEO/server/formatting_helper_functions.R")
# Find the section delimiters of the file
section_indices <- which(str_detect(func_strings, "# .+-{3}"))
# Initialize an empty list of function names
func_names <- NULL

# Loop over the sections in the file
func_lists <- lapply(1:(length(section_indices)), function(i) { # For each section
  # Extract the section
  section_end <- if (i == length(section_indices)) length(func_strings) else section_indices[i + 1] - 1
  this_section <- func_strings[section_indices[i]:section_end]
  
  # Extract the function name
  search_str <-  " ?\\<\\- ?function\\(.*"
  func_name <- str_remove(this_section[which(str_detect(this_section, search_str))], search_str)
  # Add the function name to the list of function names
  func_names <<- c(func_names, func_name)
  
  # Extract the function's library dependencies
  search_str <- "# Library dependencies: "
  lib_dependencies <- str_remove(this_section[which(str_detect(this_section, search_str))], search_str)
  lib_dependencies <- if (identical(lib_dependencies, character(0))) NA else str_split(lib_dependencies, "; ")[[1]]
  
  # Extract the function's function dependencies
  search_str <- "# Function dependencies: "
  func_dependencies <- str_remove(this_section[which(str_detect(this_section, search_str))], search_str)
  func_dependencies <- if (identical(func_dependencies, character(0))) NA else str_split(func_dependencies, "; ")[[1]]
  
  # Return a list with the pieces of the function
  list(func_text = this_section, lib_dependencies = lib_dependencies, func_dependencies = func_dependencies)
  
})

# Name the items in the list by their function name
names(func_lists) <- func_names

saveRDS(func_lists, file = "~/R_Code/TidyGEO/TidyGEO/User/rscript_functions.rds")