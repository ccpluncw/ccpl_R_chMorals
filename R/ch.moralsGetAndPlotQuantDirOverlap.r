#' A function to get and plot directional overlaps by quantity.
#'
#' This function gets and plots directional overlaps by quantity. It simply combines four other functions: ch.moralsGetDirOverlapForAllProbes(); ch.moralsQuantsToGrps(); ch.moralsGetDirOverlapByGrp(); and ch.stripPlotByGrp().
#' @param data a dataframe containing overlap data for probe combinations.
#' @param probe1Col a string the specifies the column name in "data" that contains the first probe item.
#' @param probe2Col a string the specifies the column name in "data" that contains the second probe item.
#' @param overlapCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param directionCol a string that specifies the name of the column in "data" that contains the direction column that indications the direction of the overlap.
#' @param quantValueCuts a vector of numbers that specifies the cutoff values for each quantity.  The first category is <= to the first cutoff value; each successive category (c1, c2, etc) is c1 < X <= c2; the final category is greater than the last number in the vector.
#' @param filename the filename (pdf) to save the figure.  DEFAULT = NULL (file not saved)
#' @return a dataframe with the probe in one column, it's average directional overlap in another, its N in another, followed by separate columns containing the average directional overlap and N for each group.
#' @keywords morals quantity plot directional overlaps
#' @export
#' @examples ch.moralsGetAndPlotQuantDirOverlap (overlapData, "IA1", "IB1", "overlap","direction", quantValueCuts = c(1, 7, 15), filename = "out.pdf")

ch.moralsGetAndPlotQuantDirOverlap <- function (data, probe1Col, probe2Col, overlapCol, directionCol, quantValueCuts, filename=NULL, ...) {

  data.ovrlp.1 <- ch.moralsGetDirOverlapForAllProbes(data, probe1Col, probe2Col, overlapCol, directionCol)
  data.ovrlp.2 <- ch.moralsQuantsToGrps(data.ovrlp.1, probe1Col, probe2Col, quantValueCuts)
  overOut <- ch.moralsGetDirOverlapByGrp(data.ovrlp.2, "p1", "p1GrpSize", "dirOverlap")

  overOut.names <- names(overOut[,])
  overOut.mDO.names  <- sort(grep('meanDO.', overOut.names, value=TRUE))
  overOut.mDO.lgnd <- gsub("meanDO.", "", overOut.mDO.names)
  ### these are for plotting N for each group.  Will implement later.
  # overOut.nDO.names  <- sort(grep('nDO.', overOut.names, value=TRUE))
  # overOut.nDO.lgnd <- gsub("nDO.", "", overOut.nDO.names)

  ch.stripPlotByGrp(overOut, "p1", "meanDO", overOut.mDO.names, grpLgndNames=overOut.mDO.lgnd, filename = do.filename, yLab = "Directional\nOverlap", ...)

  return(overOut)

}
