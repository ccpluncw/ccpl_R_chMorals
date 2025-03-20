#' A function to plot learning effects
#'
#' This function plots the learning function.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param trialCol a string that specifies the name of the column in "data" that contains the trial number.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param fitCol a string that specifies the name of the new column that will contain the predicted datapoints.
#' @param resCol a string that specifies the name of the new column that will contain the residual datapoints.
#' @param yLabel a string that specifies the yLabel for the graphs
#' @param filenameID a string that will be added to the file names and titles that makes them unique if you run this call several times.
#' @keywords morals learning plots
#' @return .
#' @export
#' @examples ch.moralsPlotLearningEffects (data=moralsData,"trial", "RT", "fitRT", "resRT", RTlabel)

ch.moralsPlotLearningEffect <- function (data, trialCol, RTCol, fitCol, resCol, yLabel, filenameID = "gp", ...) {

	op <- par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(5,6,4,7), cex=1, las=1)

	aveTrial <- as.data.frame(data %>% dplyr::group_by(across(all_of(trialCol))) %>% dplyr::summarise(keybRT=mean(eval(parse(text=RTCol))), fitRT = mean(eval(parse(text=fitCol))) ) )

	aveDV.fn <- paste(filenameID,"learning curve.aveDV.pdf")
	ch.plot.learning(aveTrial[[trialCol]], aveTrial[[RTCol]], fit = aveTrial$fitRT, plotTitle = paste(filenameID, "learning curve.aveDV"), filename = aveDV.fn, yLabel = yLabel, ...)

	rawDV.fn <- paste(filenameID,"learning curve.rawDV.pdf")
	ch.plot.learning(data[[trialCol]], data[[RTCol]], fit = data[[fitCol]], plotTitle = paste(filenameID, "learning curve.rawDV"), filename = rawDV.fn, yLabel = yLabel, ...)

	resid.fn <- paste(filenameID,"learning curve.resids.pdf")
	ch.plot.learning(data[[trialCol]], data[[resCol]], fit = c(0,0), plotTitle = paste(filenameID, "learning curve.resids"), filename = resid.fn, yLabel = yLabel, ...)

	par(op)

}
