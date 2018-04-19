source('geocurateFunctions_User.R')

# download metaData -------------------------------------------------------


toFilter <- NULL
toFilter[1] <- 'sameVals'
toFilter[2] <- 'allDiff'
toFilter[3] <- 'dates'
toFilter[4] <- 'reanalyzed'
toFilter[5] <- 'url'
geoID <- 'GSE1456'
metaData <- downloadClinical(geoID, toFilter)

# extract values from columns with delimiter ------------------------------


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

# exclude undesired columns -----------------------------------------------


varsToKeep <- NULL
varsToKeep[1] <- NULL
varsToKeep[0] <- NULL
allButKeep <- TRUE
metaData <- filterCols(metaData, varsToKeep, allButKeep)

# rename columns ----------------------------------------------------------


newNames <- NULL
newNames[['DEATH']] <- 'Death'
newNames[['RELAPSE']] <- 'Relapse'
newNames[['SUBTYPE']] <- 'Tumor Subtype'
metaData <- renameCols(metaData, newNames)

# substitute values -------------------------------------------------------


tablesList <- NULL
tablesList[['SURV_DEATH']] <- data.frame(To_Replace=c('RANGE: 0.18-5.7', 'RANGE: 5.7-8.49'), New_Val=c('ShortSurvival', 'LongSurvival'))
metaData <- substituteVals(metaData, tablesList)

# exclude undesired samples -----------------------------------------------


excludesList <- NULL
excludesList[['Tumor.Subtype']] <- 'No Subtype'
metaData <- excludeVars(metaData, excludesList)

# exclude undesired samples -----------------------------------------------


excludesList <- NULL
excludesList[['Tumor.Subtype']] <- 'No Subtype'
excludesList[['ELSTON']] <- 'NA'
metaData <- excludeVars(metaData, excludesList)

# save data ---------------------------------------------------------------


metaData <- cbind(rownames(metaData), metaData)
colnames(metaData)[1] <- ''
file <- '/tmp/Rtmp5TSYI9/file191124a1c78.csv'
write.csv(metaData, file, row.names = FALSE, col.names = TRUE)
