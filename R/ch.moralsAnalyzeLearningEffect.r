#' A function to analyze learning effects
#'
#' This function attempts to fit a non-linear, decelerating function to the specified data; adds the predicted and residual datapoints to the dataframe; and plots the learning function.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param trialCol a string that specifies the name of the column in "data" that contains the trial number.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param fitCol a string that specifies the name of the new column that will contain the predicted datapoints.
#' @param resCol a string that specifies the name of the new column that will contain the residual datapoints.
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @param filenameID a string that will be added to the file names and titles that makes them unique if you run this call several times.
#' @keywords morals learning analysis
#' @return a list containing: data = original datafram with the new columns; nlsFit = the nls fit object.
#' @export
#' @examples ch.moralsAnalyzeLearningEffect (data=moralsData,"trial", "RT", "fitRT", "resRT", params)

ch.moralsAnalyzeLearningEffect <- function (data, trialCol, RTCol, fitCol, resCol, params, filenameID = "gp") {

		#create new directories
		mainDir <- getwd()

		ch.newDir (mainDir, params$gpSubDir)
		gpDir <- getwd()
		setwd(mainDir)

#try learning functions to create residuals.  If each one fails, then simplify

	if (params$RTresid) {
		outList <- ch.moralsRmLearningEffect(data, trialCol,RTCol, fitCol, resCol)
		merged.trial.nls <- outList$nlsFit
		data <- outList$data
	} else {
		data[[resCol]] <- data[[RTCol]]
		data[[fitCol]] <- 0.0
	}

#	set variables for xlab
	ReactionTime <- ch.getMoralsRTaxisName (params$keybRTtransform, params$RTresid)

	op <- par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(5,6,4,7), cex=1, las=1)

	aveTrial <- as.data.frame(data %>% dplyr::group_by(across(all_of(trialCol))) %>% dplyr::summarise(keybRT=mean(eval(parse(text=RTCol))), fitRT = mean(eval(parse(text=fitCol))) ) )

	aveDV.fn <- file.path(gpDir,paste("(", params$dt.set, filenameID, ") learning curve.aveDV.pdf"))
	ch.plot.learning(aveTrial[[trialCol]], aveTrial[[RTCol]], fit = aveTrial$fitRT, plotTitle = paste(filenameID, "learning curve.aveDV"), filename = aveDV.fn, yLabel = ReactionTime)

	rawDV.fn <- file.path(gpDir,paste("(", params$dt.set,filenameID, ") learning curve.rawDV.pdf"))
	ch.plot.learning(data[[trialCol]], data[[RTCol]], fit = data[[fitCol]], plotTitle = paste(filenameID, "learning curve.rawDV"), filename = rawDV.fn, yLabel = ReactionTime)

	resid.fn <- file.path(gpDir,paste("(", params$dt.set,filenameID,") learning curve.resids.pdf"))
	ch.plot.learning(data[[trialCol]], data[[resCol]], fit = c(0,0), plotTitle = paste(filenameID, "learning curve.resids"), filename = resid.fn, yLabel = ReactionTime)

	par(op)
	outStats <- file.path(gpDir,paste("(", params$dt.set,filenameID,") learning curve stats.txt"))
	sink(outStats, append = T)
		print("**** Learning Function ****")
		print(summary(merged.trial.nls))
	sink(NULL)

	return(outList)

}
