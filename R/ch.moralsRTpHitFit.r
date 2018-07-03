#' A function to fit the RT and pHit function to morals data
#'
#' This function attempts to fit a non-linear, decelerating function to the pHit data; a linear function to the RT data;  plots the  functions on a single graph; and returns a list with the fit statistics.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param correctCol a string that specifies the name of the new column that will contains a "1" if the participant chose the item with the greatest value distribution and a "0" if they did not.
#' @param printR2 a boolean that specifies whether to print the R2 on the graphs.
#' @param filename the filename (pdf) to save the output of the graph. DEFAULT = NULL (no graph saved)
#' @keywords morals RT pHit fit plots
#' @return a list containing: RTfit = lm object with the fit of the RT data; pHitFit = the nls fit object with the fit of the pHit data; pHitR2 = the r2 of the pHitFit.
#' @export
#' @examples ch.moralsRTpHitFit (data=moralsData,"overlapRound", "resdRT", "predict", filename = "plot.pdf")

ch.moralsRTpHitFit <- function (data, overlapRoundCol, RTCol, correctCol, printR2 = F, filename = NULL, cex1 = 1.5, ...) {

	  df.tmp <- ddply(data, overlapRoundCol, summarise, aveRT = mean(eval(parse(text = RTCol))), medianRT = median(eval(parse(text = RTCol))), pHit = mean(eval(parse(text =correctCol))))
    pHitFit <- ch.plot.pHit(df.tmp[[overlapRoundCol]], df.tmp$pHit, cex1 = cex1, printR2 = printR2, yLabel  = NA, ...)
    RTfit <- ch.plot.lm(df.tmp[[overlapRoundCol]], df.tmp$aveRT, cex1 = cex1, printR2 = printR2, yLabel  = NA, ...)

    if (!is.null(filename)) {
				dev.copy(pdf, filename, width=6, height=9)
				dev.off();
		}
    return(list(RTfit = RTfit, pHitFit = pHitFit[["nlsObject"]], pHitR2 = pHitFit[["r2"]], pHitBeta = pHitFit[["beta"]]))

}
