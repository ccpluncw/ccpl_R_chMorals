#' A function to fit the RT and pHit function to morals data
#'
#' This function attempts to fit a non-linear, decelerating function to the pHit data; a linear function to the RT data;  plots the  functions on a single graph; and returns a list with the fit statistics.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param correctCol a string that specifies the name of the new column that will contains a "1" if the participant chose the item with the greatest value distribution and a "0" if they did not.
#' @param correctVals a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param minNperOverlap an integer that specifies the minimum number of trials necessary to include an overlap bin in the graph. DEFAULT = 0.
#' @param useTwoParameterModel A boolean that specifies whether to use a two parameter p(HOV) model.  If this is set to TRUE, then this function will fit a p(HVO) model whereby the rightmost point (overlap = 1.0) is not fixed at p(HVO) = 0.5. DEFAULT = FALSE.
#' @param cex1 sets the default font size. DEFAULT = 1.25.
#' @param cex.topTile sets the default font size of the title at the top of the page. DEFAULT = 1.25.
#' @param printR2 a boolean that determines whether to print the r square on the graph. DEFAULT = FALSE.
#' @param topTitle a string that will be the title at the top of the page. DEFAULT = NULL.
#' @param filename the filename (pdf) to save the output of the graph. DEFAULT = NULL (no graph saved)
#' @param minUniqueOverlaps An integer specifying the minimum number of unique overlap bins necessary for the program to calculate the pHVO and RT function.  DEFAULT = 2.
#' @keywords morals RT pHit fit plots
#' @return a list containing: RTfit = lm object with the fit of the RT data; pHitFit = the nls fit object with the fit of the pHit data; pHitR2 = the r2 of the pHitFit.
#' @export
#' @examples ch.moralsRTpHitFit (data=moralsData,"overlapRound", "resdRT", "correct", c("yes", "no"), filename = "plot.pdf")

ch.moralsRTpHitFit <- function (data, overlapRoundCol, RTCol, correctCol, correctVals = c(TRUE, FALSE), minNperOverlap = 0, useTwoParameterModel = FALSE, cex1 = 1.5, cex.topTile =1.25, printR2 = F, topTitle = NULL, filename = NULL, minUniqueOverlaps = 2, ...) {

		data$correct01 <- ifelse (data[[correctCol]]==correctVals[1], 1, 0)

		if (!is.null(filename)) {
			op <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.25, las=1)
		}

		#get summarized data
		df.tmp.1 <- as.data.frame(data %>% dplyr::group_by(across(all_of(overlapRoundCol))) %>% dplyr::summarise(aveRT = mean(eval(parse(text = RTCol)), na.rm=T), medianRT = median(eval(parse(text = RTCol)), na.rm=T), pHit = mean(correct01, na.rm=T), n =sum(!is.na(correct01)) ) )

		#filter out overlaps that are too small
		df.tmp <- df.tmp.1[df.tmp.1$n > minNperOverlap,]

		#calculate the number of unique overlap bins
		nUniqueOverlapBins <- length(unique(df.tmp[[overlapRoundCol]]))

		if(nUniqueOverlapBins >= minUniqueOverlaps) {
			#get x axis limits
			minX <- floor(min(df.tmp[[overlapRoundCol]]))
			maxX <- ceiling(max(df.tmp[[overlapRoundCol]]))
			xLims <- c(minX, maxX)

	    pHitFit <- ch.plot.pHit(df.tmp[df.tmp$n > minNperOverlap, overlapRoundCol], df.tmp[df.tmp$n > minNperOverlap, "pHit"], useTwoParameterModel = useTwoParameterModel, cex1 = cex1, printR2 = printR2, yLabel  = NA, xlim = xLims, ...)
	    RTfit <- ch.plot.lm(df.tmp[df.tmp$n > minNperOverlap, overlapRoundCol], df.tmp[df.tmp$n > minNperOverlap, "aveRT"], cex1 = cex1, printR2 = printR2, yLabel  = NA, xlim = xLims, ...)
			if(!is.null(topTitle)) {
	      mtext(topTitle, outer = TRUE, cex = cex.topTile)
	    }

	    if (!is.null(filename)) {
					dev.copy(pdf, filename, width=6, height=9)
					dev.off();
					par(op)
			}

	    return(list(RTfit = RTfit, pHitFit = pHitFit[["nlsObject"]], pHitR2 = pHitFit[["r2"]], pHitBeta = pHitFit[["beta"]], pHitAlpha = pHitFit[["alpha"]]))
		} else {
			return (list(RTfit = NA, pHitFit = NA, pHitR2 = NA, pHitBeta = NA, pHitAlpha = NA))
		}

}
