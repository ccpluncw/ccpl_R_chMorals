#' A function to cleans the choice data for analysis
#'
#' This function cleans the choice data for analysis by removing bad subjects (based on sn column), dropping columns, making sure the trials starts at 1, and computing average p(HVO) and RT for each subject.
#' @param data A dataframe containing the choice data.
#' @param snCol a string that specifies the name of the column in "data" that contains the subject number.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param trialCol a string that specifies the name of the column in "data" that contains the trial number.
#' @param respChoiceCol a string that specifies the name of the column in "data" that contains the the participant's response to the prompt - yes take action or no take no action.
#' @param respChoiceVal a vector of two values that specifies the choose Item1 option ("yes" take action in many morals experimants, thus saving Item1) value (index 1) and the choose Item1 option ("no" take no action in many morals experimants, thus saving Item2) value (index 2).
#' @param dropVarFilename A filename of a file that contains a vector of column names to be removed from "data" DEFAULT = NULL (remove nothing)
#' @param dropSNFilename A filename of a file that contains a vector of subject names to be removed from "data" DEFAULT = NULL (remove nothing)
#' @param RTlogTransform A boolean to specify whether to transform the RT variable to log(RT). DEFAULT = FALSE
#' @param outputFile the filename that you want the output written to. DEFAULT = NULL (no file written)
#' @keywords choice data clean
#' @return a dataframe of cleaned data.
#' @export
#' @examples ch.moralsDataClean (data=moralsData, "sn", "RT", "trials", "keyDef", dropVarFilename ="dropVars.txt", dropSNFilename = "badSNs.txt")

ch.moralsDataClean  <- function (data, snCol, RTcol, trialCol, respChoiceCol, respChoiceVal = c("Item1", "Item2"), dropVarFilename = NULL, dropSNFilename = NULL, RTlogTransform = FALSE, outfile = NULL) {

	if(!is.null(dropVarFilename)) {
		drops <- scan(dropVarFilename, what="a")
		#remove extra columns specified in dropVarFile
		data <- data[ , !(names(data) %in% drops)]
	}

	#### remove subjects who had computer troubles or did not comply with the cell phone use instructions
	if(!is.null(dropSNFilename)) {
		data <- ch.removeBadSNs(data, snCol, dropSNFilename)
	}

	#### only keep data in which the participant responds yes or no. Get rid of bad button presses
	data <- data[data[[respChoiceCol]]==respChoiceVal[1] | data[[respChoiceCol]]==respChoiceVal[2], ]

	#add 1 to trials if they start at 0
	if (min(data[[trialCol]])==0) {
		data[[trialCol]] <- data[[trialCol]] + 1
	}

	#	log transform RT
	if (RTlogTransform) {
		#remove any 0ms rts
		data[[RTcol]] <- ifelse(data[[RTcol]] <1, 1, data[[RTcol]])
		data[[RTcol]] <- log(data[[RTcol]])
	}

	if(!is.null(outfile)) {
    write.table(data, file=outfile, quote=F, sep="\t", row.names=F)
  }

	return (data)
}
