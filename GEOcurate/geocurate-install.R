if (FALSE) {
devtools::install_github('andrewsali/shinycssloaders')
install.packages("shiny")
install.packages("DT")
#install.packages("shinyjs")
install.packages("shinyBS")
install.packages("rhandsontable")

source("https://bioconductor.org/biocLite.R")
biocLite("GEOquery")

source("https://bioconductor.org/biocLite.R")
biocLite("BiocUpgrade")

install.packages("dplyr")
install.packages("stringr")
install.packages("readr")

install.packages("RCurl")
#install.packages("digest")
install.packages("yaml")

install.packages("feather")
devtools::install_github("AnalytixWare/ShinySky")

install.packages("shinyFiles")

install.packages("xlsx")

bioc <- local({
  env <- new.env()
  on.exit(rm(env))
  evalq(source("http://bioconductor.org/biocLite.R", local = TRUE), env)
  biocinstallRepos()
})

}
