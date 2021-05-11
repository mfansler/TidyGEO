install.packages(c("shiny", "tidyverse", "devtools", "assertive", "data.table", "DT", "feather", "fs", "httr", "janitor", "later", "plotly", "Rcpp", "RColorBrewer", "RCurl", "rJava", "rhandsontable", "rlang", "shinyBS", "shinycssloaders", "shinydashboard", "shinyjs", "shinyWidgets", "xlsx", "xml2", "yaml"), repos="https://cloud.r-project.org")

devtools::install_github('AnalytixWare/ShinySky')

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("GEOquery")
