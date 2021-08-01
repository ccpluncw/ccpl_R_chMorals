#' A function to plot the subject fits
#'
#' This function plots the subject fits.
#' @param snSummaryArray a dataframe containing the morals fit coeficients and r square for RT and p(Hit) for each subject.
#' @param snCol a string that specifies the name of the column in "snSummaryArray" that contains the subject number.
#' @param rtSloCol a string that specifies the name of the column in "snSummaryArray" that contains the best fit slope for the RT data of each subject.
#' @param rtIntCol a string that specifies the name of the column in "snSummaryArray" that contains the best fit intercept for the RT data of each subject.
#' @param rtR2Col a string that specifies the name of the column in "snSummaryArray" that contains the r square the fit of the RT data of each subject.
#' @param phBCol a string that specifies the name of the column in "snSummaryArray" that contains the best fit beta for the p(hit) data of each subject.
#' @param phACol a string that specifies the name of the column in "snSummaryArray" that contains the best fit alpha for the p(hit) data of each subject.
#' @param phR2Col a string that specifies the name of the column in "snSummaryArray" that contains the r square the fit of the p(hit) data of each subject.
#' @param xAll a vector containing the overlap rounds used when fitting the RT and p(hit) data.
#' @param filename a string with the filename (pdf) for the plot to be saved. DEFAULT = NULL (no file saved)
#' @keywords morals subject fit plots
#' @return .
#' @export
#' @examples ch.moralsPlotSnRTpHitFits(subOutData, "sn", "rtSlo", "rtInt", "rtR2", "phB", "phA", "phR2", xAll, filename)

ch.moralsPlotSnRTpHitFits <- function (snSummaryArray, snCol, rtSloCol, rtIntCol, rtR2Col, phBCol, phACol, phR2Col, xAll, filename = NULL) {

  op <- par(mfrow=c(2,2),bty="n", font=1, family='serif', mar=c(2,3,3,2), oma=c(3,3,3,3), cex=1.5, las=1)

  #create a dummy y dataset that will be used to create the initial graph and printed in white so it wont be seen
  yAll <- rep(0,length(xAll))
  snSummaryArray <- snSummaryArray[order(abs(snSummaryArray[[rtR2Col]])),]

  Y1 <- snSummaryArray[[rtIntCol]] + snSummaryArray[[rtSloCol]]*max(xAll)
  Y2 <- snSummaryArray[[rtIntCol]] + snSummaryArray[[rtSloCol]]*min(xAll)
  maxY <- max (c(Y1, Y2))
  minY <- min (c(Y1, Y2))

  # get y min and max bounds
  buffer <- (maxY - minY) * .1
  ylimMin <-  minY - buffer
  ylimMax <-  maxY + buffer

  #create graph with a dummy dataset that cant be seen (drawn in white)
  plot(xAll, yAll, main="Slopes", xlab= NA, ylab=NA, type="l", col="white", lwd=1, ylim=c(ylimMin,ylimMax))

  #add data
  for (j in 1:length(snSummaryArray[[snCol]]))  {
    lineCol <- rgb(1-abs(snSummaryArray[[rtR2Col]][j]),1-abs(snSummaryArray[[rtR2Col]][j]),1-abs(snSummaryArray[[rtR2Col]][j]))
    abline(a=snSummaryArray[[rtIntCol]][j], b=snSummaryArray[[rtSloCol]][j], col=lineCol, lwd=1)
  }

  snSummaryArray <- snSummaryArray[order(snSummaryArray[[phR2Col]]),]
  yAll2 <- rep(0.5,length(xAll))

  #create graph with a dummy dataset that cant be seen (drawn in white)
  plot(xAll, yAll2, main="p(hit)", xlab= NA, ylab=NA, type="l", col=lineCol, lwd=1, ylim = c(0,1))
  abline(a=0.5,b=0,col="grey", lwd=2)

  #add data
  for (j in 1:length(snSummaryArray[[snCol]]))  {
    if(!is.na(snSummaryArray[[phR2Col]][j])) {
#      y <- .5*(1-(xAll^snSummaryArray[[phBCol]][j]))+.5
      y <- (1-snSummaryArray[[phACol]][j])*(1-(xAll^snSummaryArray[[phBCol]][j]))+snSummaryArray[[phACol]][j]
      lineCol <- rgb(1-snSummaryArray[[phR2Col]][j],1-snSummaryArray[[phR2Col]][j],1-snSummaryArray[[phR2Col]][j])
      lines(xAll, y, col=lineCol, lwd=1)
    }
  }

  par(lwd=2)
  hist(snSummaryArray[[rtR2Col]], main=NA, xlab= NA, ylab = NA,  xlim = c(-1,1), breaks=12, col=NULL)
  hist(snSummaryArray[[phR2Col]], main=NA, xlab= NA, ylab = NA,  xlim = c(0,1), breaks=12, col=NULL)

  if(!is.null(filename)) {
    dev.copy(pdf, filename, width=12, height=9)
    dev.off();
  }

  par(op)
}
