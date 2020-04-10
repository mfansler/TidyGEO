if (FALSE) {
devtools::install_github('andrewsali/shinycssloaders')
install.packages("shiny")
install.packages("DT")
install.packages("shinyjs")
install.packages("shinyBS")
install.packages("rhandsontable")

source("https://bioconductor.org/biocLite.R")
BiocInstaller::biocLite("GEOquery")

source("https://bioconductor.org/biocLite.R")
BiocInstaller::biocLite("BiocUpgrade")
install.packages("BiocManager")
library("BiocManager")
BiocManager::install("BiocUpgrade")

install.packages("dplyr")
install.packages("stringr")
install.packages("readr")

install.packages("RCurl")
#install.packages("digest")
install.packages("yaml")

devtools::install_github("wesm/feather/R")
install.packages("devtools")
devtools::install_github("AnalytixWare/ShinySky")

install.packages("shinyFiles")

install.packages("xlsx")

bioc <- local({
  env <- new.env()
  on.exit(rm(env))
  evalq(source("http://bioconductor.org/biocLite.R", local = TRUE), env)
  biocinstallRepos()
})

install.packages("Rcpp")
install.packages("later")

install.packages("installr")
library(installr)

install.packages("DT")
install.packages("RColorBrewer")
install.packages("assertive")
install.packages("data.table")
install.packages("fs")
install.packages("rJava")
install.packages("rdrop2")
install.packages("splitstackshape")
install.packages("shinyWidgets")
install.packages("rlang")
install.packages("xml2")
#install.packages("colourpicker")
}
