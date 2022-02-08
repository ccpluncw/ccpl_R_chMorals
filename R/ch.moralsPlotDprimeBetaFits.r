#' A function to plot the dPrime and Beta by OverlapRound for the Morals data.
#'
#' This function plots the dPrime and Beta by OverlapRound for the Morals data.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlap round column.
#' @param correctCol a string the specifies the column name in "data" that contains the variable whether the participant's response was correct.
#' @param correctVals a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param targetPresentCol a string that specifies the name of the column in "data" that contains the whether or not the target was presented.
#' @param targetPresentVals a vector of two values that specifies the "target present" value (index 1) and the "target absent" value (index 2). e.g, c("yes", "no")
#' @param addCorrection a boolean that specifies whether you want a .5 correction to be added the total hits, FAs, misses, and CRs. This corrects for 0 and 1 values for FA and Hits. DEFAULT = T.
#' @param minNperOverlap an integer that specifies the minimum number of trials necessary to include an overlap bin in the graph. DEFAULT = 0.
#' @param minUniqueOverlaps An integer specifying the minimum number of unique overlap bins necessary for the program to calculate the dPrime function.  DEFAULT = 3.
#' @param cex1 sets the default font size. DEFAULT = 1.25.
#' @param cex.topTile sets the default font size of the title at the top of the page. DEFAULT = 1.25.
#' @param printR2 a boolean that determines whether to print the r square on the graph. DEFAULT = FALSE.
#' @param topTitle a string that will be the title at the top of the page. DEFAULT = NULL.
#' @param filename the filename (pdf) to save the output of the graph. DEFAULT = NULL (no graph saved)
#' @keywords morals data plot dPrime beta
#' @return a list containing the lm fit for the dPrime by OverlapRound (dPrimeFit) and the lm fit for the beta by OverlapRound (betaFit).
#' @export
#' @examples ch.moralsPlotDprimeBetaFits (data=moralsData,"overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), filename = "myplot.pdf")

ch.moralsPlotDprimeBetaFits <- function (data, overlapRoundCol, correctCol, correctVals, targetPresentCol, targetPresentVals, addCorrection = TRUE, minNperOverlap = 0, minUniqueOverlaps = 3, cex1 = 1.25, cex.topTile =1.25, printR2 = T, topTitle = NULL, filename = NULL, ...) {

    df.tmp <- ch.calculateDprimeStats(data, overlapRoundCol, correctCol, correctVals, targetPresentCol, targetPresentVals, addCorrection = TRUE)

    df.dPrime <- df.tmp[df.tmp$N.targetPresent > minNperOverlap & df.tmp$N.targetAbsent > minNperOverlap,]

    uniqueOverlapsN <- length(unique(df.dPrime[[overlapRoundCol]]))
    #run the analysis if there are at least three overlap levels
    if(uniqueOverlapsN >= minUniqueOverlaps) {

      #plot d prime and beta by overlap round
      op <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.25, las=1)

      #get x axis limits
      minX <- floor(min(df.dPrime[[overlapRoundCol]]))
      maxX <- ceiling(max(df.dPrime[[overlapRoundCol]]))
      xLims <- c(minX, maxX)

      dPrimeFit <- ch.plot.lm(df.dPrime[[overlapRoundCol]], df.dPrime$dPrime, cex1 = cex1, printR2 = printR2, yLabel  = "d'", xlim = xLims, ...)
      betaFit <- ch.plot.lm(df.dPrime[[overlapRoundCol]], df.dPrime$beta, cex1 = cex1, printR2 = printR2, yLabel  = 'beta', xlim = xLims, ...)
      if(!is.null(topTitle)) {
        mtext(topTitle, outer = TRUE, cex = cex.topTile)
      }
      if (!is.null(filename)) {
          dev.copy(pdf, filename, width=6, height=9)
          dev.off();
      }

      par(op)

      outList <- list(dPrimeFit = dPrimeFit, betaFit = betaFit)
    } else {
      outList <- list(dPrimeFit = NA, betaFit = NA)
    }
    return (outList)
}
