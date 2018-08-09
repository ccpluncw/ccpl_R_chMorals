#' A function to fit the RT and pHit function to morals data
#'
#' This function attempts to fit a non-linear, decelerating function to the pHit data; a linear function to the RT data;  plots the  functions on a single graph; and returns a list with the fit statistics.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param correctCol a string that specifies the name of the new column that will contains a "1" if the participant chose the item with the greatest value distribution and a "0" if they did not.
#' @param correctVals a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param cex1 sets the default font size. DEFAULT = 1.25.
#' @param cex.topTile sets the default font size of the title at the top of the page. DEFAULT = 1.25.
#' @param printR2 a boolean that determines whether to print the r square on the graph. DEFAULT = FALSE.
#' @param topTitle a string that will be the title at the top of the page. DEFAULT = NULL.
#' @param filename the filename (pdf) to save the output of the graph. DEFAULT = NULL (no graph saved)
#' @keywords morals RT pHit fit plots
#' @return a list containing: RTfit = lm object with the fit of the RT data; pHitFit = the nls fit object with the fit of the pHit data; pHitR2 = the r2 of the pHitFit.
#' @export
#' @examples ch.moralsRTpHitFit (data=moralsData,"overlapRound", "resdRT", "correct", c("yes", "no"), filename = "plot.pdf")

ch.moralsRTpHitFit <- function (data, overlapRoundCol, RTCol, correctCol, correctVals = c(TRUE, FALSE), cex1 = 1.5, cex.topTile =1.25, printR2 = F, topTitle = NULL, filename = NULL, ...) {

		data$correct01 <- ifelse (data[[correctCol]]==correctVals[1], 1, 0)

		if (!is.null(filename)) {
			op <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.25, las=1)
		}

		df.tmp <- as.data.frame(data %>% dplyr::group_by_(overlapRoundCol) %>% dplyr::summarise(aveRT = mean(eval(parse(text = RTCol))), medianRT = median(eval(parse(text = RTCol))), pHit = mean(correct01) ) )

    pHitFit <- ch.plot.pHit(df.tmp[[overlapRoundCol]], df.tmp$pHit, cex1 = cex1, printR2 = printR2, yLabel  = NA, ...)
    RTfit <- ch.plot.lm(df.tmp[[overlapRoundCol]], df.tmp$aveRT, cex1 = cex1, printR2 = printR2, yLabel  = NA, ...)
		if(!is.null(topTitle)) {
      mtext(topTitle, outer = TRUE, cex = cex.topTile)
    }

    if (!is.null(filename)) {
				dev.copy(pdf, filename, width=6, height=9)
				dev.off();
				par(op)
		}

    return(list(RTfit = RTfit, pHitFit = pHitFit[["nlsObject"]], pHitR2 = pHitFit[["r2"]], pHitBeta = pHitFit[["beta"]]))

}
