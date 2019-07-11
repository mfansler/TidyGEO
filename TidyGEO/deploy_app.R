

# Updating series information ---------------------------------------------

update_series <- menu(c("Yes", "No"), title = "Update series list to match what's currently on GEO (this will take some time)?")

if (update_series == 1) {
  source("~/R_Code/TidyGEO/TidyGEO/series_platform/Combine_Series_Platforms.R")
}


# Generating functions for Rscript writing --------------------------------

source("~/R_Code/TidyGEO/TidyGEO/generate_rscript_functions.R")


# Setting Bioconductor repositories ---------------------------------------

local({
  source("http://bioconductor.org/biocLite.R")
  options(repos = BiocInstaller::biocinstallRepos())
})


# Deploying app -----------------------------------------------------------

library(rsconnect)
deployApp(appDir = "~/R_Code/TidyGEO/TidyGEO", appName = "tidygeo", appTitle = "TidyGEO", appFileManifest = "~/R_Code/TidyGEO/TidyGEO/file_manifest.txt", forceUpdate = T, launch.browser = T)
