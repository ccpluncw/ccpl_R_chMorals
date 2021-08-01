#' A function remove the learning effect on RT by Subject
#'
#' This function remove the learning effect on RT by Subject.
#' @param data A dataframe containing the choice data (often after running through ch.moralsDataPrep()).
#' @param snCol A string that specifies the name of the column in "data" that contains the subject number.
#' @param trialCol A string that specifies the name of the column in "data" that contains the trial number.
#' @param RTcol A string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param fitCol A string that specifies the name of the new column that will contain the predicted datapoints.
#' @param resCol A string that specifies the name of the new column that will contain the residual datapoints.
#' @param outputFileTag A string to put on the front of a filename that you want a graph of the learning fit written to. DEFAULT = NULL (no file written)
#' @keywords learning RT remove by subject
#' @return A dataframe with the learning function fit and residuals included
#' @export
#' @examples ch.rmLearningBySN (data=moralsData,"sn", "trial", "RT", "res.RT", "fit.RT", "fitOut.pdf")


ch.rmLearningBySN <- function (data, snCol, trialCol, RTcol, fitCol, resCol, outputFile = NULL) {

		#identify our subjects
		subs <- unique (data[[snCol]])

		subData <- NULL
		for (j in subs) {
			outList <- ch.moralsRmLearningEffect(data[data[[snCol]] == j,], trialCol,RTcol, fitCol, resCol)
			subData <- ch.rbind (subData, outList$data)
		}

    if(!is.null(outputFile)) {
      op1 <-	par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(5,6,4,7), las=1, cex=1)
  		ReactionTimeLabel <- "RTres"
  		ch.moralsPlotLearningEffect(subData, trialCol, RTcol, fitCol, resCol, yLabel = ReactionTimeLabel, filenameID = outputFile, cex1 = 1)
      par(op1)
    }


		return(subData)
}
