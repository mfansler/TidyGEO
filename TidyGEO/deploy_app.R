update_series <- menu(c("Yes", "No"), title = "Update series list to match what's currently on GEO (this will take some time)?")

if (update_series == 1) {
  source("~/R_Code/TidyGEO/TidyGEO/series_platform/Combine_Series_Platforms.R")
}

local({
  source("http://bioconductor.org/biocLite.R")
  options(repos = BiocInstaller::biocinstallRepos())
})

library(rsconnect)
deployApp(appDir = "~/R_Code/TidyGEO/TidyGEO", appName = "tidygeo", appTitle = "TidyGEO", appFileManifest = "~/R_Code/TidyGEO/TidyGEO/file_manifest.txt", forceUpdate = T, launch.browser = T)
