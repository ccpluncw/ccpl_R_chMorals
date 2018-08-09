#' A function to analyze the group Morals data
#'
#' This function analyzes the group  morals data.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param trialCol a string that specifies the name of the column in "data" that contains the trial number.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param fitCol a string that specifies the name of the new column that will contain the predicted datapoints.
#' @param resCol a string that specifies the name of the new column that will contain the residual datapoints.
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param yesNoCol a string the specifies the column name in "data" that contains the variable with the participant's yes/no response.
#' @param yesNoVal a vector of two values that specifies the yes "take action" value (index 1) and the no "take no action" value (index 2). e.g, c("yes", "no")
#' @param correctCol a string that specifies the name of the new column that will contains a "1" if the participant chose the item with the greatest value distribution and a "0" if they did not.
#' @param correctVals a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @keywords morals group data analysis
#' @return dataframe with the learning function fit and residuals
#' @export
#' @examples ch.moralsGrpRTpHit (data=moralsData,"trial", "RT", "res.RT", "fit.RT", "overlap", "keyDef", c("Yes", "No"), "correct", c("yes", "no"), params=parameters)

ch.moralsGrpRTpHit <- function (data, trialCol, RTCol, fitCol, resCol, overlapRoundCol, yesNoCol, yesNoVal = c("Yes", "No"), correctCol, correctVals = c(TRUE, FALSE), params) {

		#create new directories
		mainDir <- getwd()

		ch.newDir (mainDir, params$gpSubDir)
		gpDir <- getwd()
		setwd(mainDir)

		statsOutputFile <- file.path(mainDir,paste(params$dt.set, params$statsOutputFilePrefix))

		if (params$RTresid) {
			### remove learning effect and output stats
			outList <- ch.moralsRmLearningEffect(data, trialCol,RTCol, fitCol, resCol)
				learning.fit <- outList$nlsFit
				data <- outList$data
				sink(statsOutputFile, append = T)
					cat("\n\n**** Learning Function ****\n\n")
					print(summary(learning.fit))
				sink(NULL)

			### plot learning effect
			op <-	par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(5,6,4,7), las=1, cex=1)
			setwd(gpDir)
			ReactionTimeLabel <- ch.getMoralsRTaxisName(params$keybRTtransform, params$RTresid)
			ch.moralsPlotLearningEffect(data, trialCol, RTCol, fitCol, resCol, yLabel = ReactionTimeLabel, filenameID = paste(params$dt.set,"gp"), cex1 = 1)
			setwd(mainDir)
		} else {
			data[[resCol]] <- data[[RTCol]]
			data[[fitCol]] <- 0.0
		}

		##### look at the probability of a No response
		merged.probno <-file.path(gpDir,paste(params$dt.set,"gp p(no).pdf"))
		fitno <- ch.moralsGetProbNo(data, overlapRoundCol, yesNoCol, yesNoVal, plotFilename = merged.probno)
		sink(statsOutputFile, append = T)
			cat("\n\n**** p(No) ****\n\n")
			print(summary(fitno))
		sink(NULL)

		### fit RT and pHit data
		plotFilename <- file.path(gpDir,paste(params$dt.set,"gp rt p(Hit).pdf"))
		outList <- ch.moralsRTpHitFit(data, overlapRoundCol, resCol, correctCol, correctVals, filename = plotFilename)
		sink(statsOutputFile, append = T)
			cat("\n\n**** Average RT ****\n\n")
			print(summary(outList$RTfit))
			cat("\n\n**** p(Hit) ****\n\n")
			print(summary(outList$pHitFit))
			cat("r_square: ",outList$pHitR2)
		sink(NULL)
		par(op)

		return (data)
}
