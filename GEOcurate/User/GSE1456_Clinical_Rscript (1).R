source('geocurateFunctions_User.R')
toFilter <- NULL
toFilter[1] <- 'sameVals'
toFilter[2] <- 'allDiff'
toFilter[3] <- 'dates'
toFilter[4] <- 'reanalyzed'
toFilter[5] <- 'url'
geoID <- 'GSE1456'
metaData <- downloadClinical(geoID, toFilter)
colsToSplit <- NULL
colsToSplit[1] <- NULL
colsToSplit[0] <- NULL
colsToDivide <- NULL
colsToDivide[1] <- NULL
colsToDivide[0] <- NULL
toSplit <- TRUE
toDivide <- FALSE
delimiter <- ': '
delimiter2 <- ''
allButSplit <- TRUE
allButDivide <- FALSE
metaData <- extractCols(metaData, toSplit, colsToSplit, toDivide, colsToDivide, delimiter, delimiter2, allButSplit, allButDivide)
newNames <- NULL
newNames[['DEATH']] <- 'Outcome Death'
metaData <- renameCols(metaData, newNames)
tablesList <- NULL
tablesList[['SUBTYPE']] <- data.frame(To_Replace=c('No Subtype'), New_Val=c('NA'))
tablesList[['SURV_DEATH']] <- data.frame(To_Replace=c('RANGE: 0.18-4.1'), New_Val=c('ShortSurvival'))
metaData <- substituteVals(metaData, tablesList)
excludesList <- NULL
excludesList[['SUBTYPE']] <- 'NA'
metaData <- excludeVars(metaData, excludesList)
