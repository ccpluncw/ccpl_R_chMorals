#' A function to prepare the morals data for analysis
#'
#' This function prepares the morals data for analysis by creating overlapRound, correct, and a few other columns.  It also makes sure the trials starts at 1, and computes average p(hit) and RT for each subject.
#' @param data morals dataframe.
#' @param snCol a string that specifies the name of the column in "data" that contains the subject number.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param overlapCol a string that specifies the name of the column in "data" that contains the overlaps for the item in each trial.
#' @param directionCol a string that specifies the name of the column in "data" that contains the direction of the overlap for each trial.
#' @param trialCol a string that specifies the name of the column in "data" that contains the trial number.
#' @param respChoiceCol a string that specifies the name of the column in "data" that contains the the participant's response to the prompt - yes take action or no take no action.
#' @param respChoiceVal a vector of two values that specifies the choose Item1 option ("yes" take action in many morals experimants, thus saving Item1) value (index 1) and the choose Item1 option ("no" take no action in many morals experimants, thus saving Item2) value (index 2).
#' @param item1cols a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 1.
#' @param item2cols a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 2.
#' @param overlapItem1cols a vector of strings that specifies the names of the columns in the overlaps file that contains the correspoinding probes in Item 1.
#' @param overlapItem2cols a vector of strings that specifies the names of the columns in the overlaps file that contains the correspoinding probes in Item 2.
#' @param statsOutputFile the filename that you want the statistics summary output written to. DEFAULT = NULL (construct filename from "params")
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @param overlapDataIsComplete A boolean that specifies whether the overlap data contains all the possible trial stimuli in the exact column order (TRUE) or whether the overlap file contains the trial stimuli in only one order and therefore has to be permuted to be merged with the choice data (FALSE).  DEFAULT = FALSE.
#' @keywords morals data prep
#' @return a dataframe of prepared data.  It also writes the data to prepDataOutFile (specified in params) which will be used by other functions.
#' @export
#' @import chutils
#' @importFrom dplyr %>%
#' @examples ch.moralsDataPrep (data=moralsData, "sn", "RT", "overlap", "direction", "trials", "respDef", respChoiceVal = c("Yes", "No"), params=parameters)

ch.moralsDataPrep  <- function (data, snCol, RTcol, overlapCol, directionCol, trialCol, respChoiceCol, respChoiceVal = c("Item1", "Item2"), item1cols = c("Item1"), item2cols = c("Item2"), overlapItem1cols = c("IA1"), overlapItem2cols = c("IB1"),statsOutputFile = NULL, params, overlapDataIsComplete = FALSE) {


	transformRT <- ifelse (params$keybRTtransform == "log", TRUE, FALSE)

	df.out <- ch.moralsDataClean(data, snCol = snCol, RTcol = RTcol, trialCol = trialCol, respChoiceCol = respChoiceCol , respChoiceVal = respChoiceVal, dropVarFilename =params$dropVarFile, dropSNFilename = params$removeBadSNFile, transformRT,  outfile = NULL)

	dat.over <-read.table(params$valueOverlapDataFile, sep="\t", header=T, quote="\"")

	df.out <- ch.mergeChoiceDataWithOverlapsData(df.out, dat.over, overlapCol = overlapCol, directionCol = directionCol, respChoiceCol = respChoiceCol, respChoiceVal = respChoiceVal, item1cols = item1cols, item2cols = item2cols, overlapItem1cols = overlapItem1cols, overlapItem2cols = overlapItem2cols,outfile = params$prepDataOutFile, roundThreshold = params$roundThreshold, roundDirection = params$roundDirection, overlapDataIsComplete = overlapDataIsComplete)

	if(is.null(statsOutputFile)) {
		statsOutputFile <- paste(params$dt.set, params$statsOutputFilePrefix)
	}

	sink(statsOutputFile, append = T)
		cat("\n\nOverlap Round Threshold:")
		cat("\n\t",params$roundThreshold)
		cat("\n\nTransform RT Into:")
		cat("\n\t",params$keybRTtransform)
	sink(NULL)


		return (df.out)
}
