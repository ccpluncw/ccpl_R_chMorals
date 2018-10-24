#' A function to read the morals dbfile
#'
#' This function reads the dbfile for the morals analysis.  The dbfile contains key words and their values for the analysis.
#' @param dbfile the filename of the dbfile.
#' @keywords morals dbfile read
#' @return a list with all the parameters
#' @export
#' @examples ch.readMoralsDBfile ("myDBfile.txt")

ch.readMoralsDBfile <- function (dbfile = "moralsDBfile.txt") {

##### Functions ######
		params<-read.table(dbfile,row.names=1,header = F,sep=",")

#### Filenames
		moralsTaskDataFile <- as.character(params['moralsTaskDataFile',1])
		valueOverlapDataFile <- as.character(params['valueOverlapDataFile',1])
		prepDataOutFile <- as.character(params['prepDataOutFile',1])
		analysisReadyOutFile <- as.character(params['analysisReadyOutFile',1])
		statsOutputFilePrefix <- as.character(params['statsOutputFilePrefix',1])
		removeBadSNFile <- as.character(params['removeBadSNFile',1])
		dropVarFile <- as.character(params['dropVarFile',1])

######_____SET SWITCHES AND THRESHOLDS______######
		lowRTthreshold <- as.numeric(as.character(params['lowRTthreshold',1]))
		highRTthreshold <- as.numeric(as.character(params['highRTthreshold',1]))
		highAveThresholdRT <- as.numeric(as.character(params['highAveThresholdRT',1]))
		lowAveThresholdRT <- as.numeric(as.character(params['lowAveThresholdRT',1]))
		chanceThreshold <- as.numeric(as.character(params['chanceThreshold',1]))
		roundThreshold <- as.numeric(as.character(params['roundThreshold',1]))
    roundThresholdSN <- as.numeric(as.character(params['roundThresholdSN',1]))
    minOverlapN <- as.numeric(as.character(params['minOverlapN',1]))
    numPlotRows <- as.numeric(as.character(params['numPlotRows',1]))
		roundDirection <- eval(parse(text=as.character(params['roundDirection',1])))
		keybRTtransform = as.character(params['keybRTtransform',1])
		RTresid = as.logical(as.character(params['RTresid',1]))
    RTresidSN = as.logical(as.character(params['RTresidSN',1]))
		dt.set = as.character(params['dt.set',1])

		minGroupAnum <- as.numeric(as.character(params['minGroupAnum',1]))
		maxGroupAnum <- as.numeric(as.character(params['maxGroupAnum',1]))
		minGroupBnum <- as.numeric(as.character(params['minGroupBnum',1]))
		maxGroupBnum <- as.numeric(as.character(params['maxGroupBnum',1]))

### get sub directories
		snSubDir <- as.character(params['snSubDir',1])
		gpSubDir <- as.character(params['gpSubDir',1])
		itemSubDir <- as.character(params['itemSubDir',1])

    paramList <- list (
      moralsTaskDataFile = moralsTaskDataFile,
      valueOverlapDataFile = valueOverlapDataFile,
			prepDataOutFile = prepDataOutFile,
			analysisReadyOutFile = analysisReadyOutFile,
			statsOutputFilePrefix = statsOutputFilePrefix,
      removeBadSNFile = removeBadSNFile,
			dropVarFile = dropVarFile,
      lowRTthreshold = lowRTthreshold,
      highRTthreshold = highRTthreshold,
      highAveThresholdRT = highAveThresholdRT,
      lowAveThresholdRT = lowAveThresholdRT,
      chanceThreshold = chanceThreshold,
      roundThreshold = roundThreshold,
      roundThresholdSN = roundThresholdSN,
      roundDirection = roundDirection,
      minOverlapN = minOverlapN,
      numPlotRows = numPlotRows,
      keybRTtransform = keybRTtransform,
      RTresid = RTresid,
      RTresidSN = RTresidSN,
      dt.set = dt.set,
      minGroupAnum = minGroupAnum,
      maxGroupAnum = maxGroupAnum,
      minGroupBnum = minGroupBnum,
      maxGroupBnum = maxGroupBnum,
      snSubDir = snSubDir,
      gpSubDir = gpSubDir,
      itemSubDir = itemSubDir
      )

    return(paramList)

}
