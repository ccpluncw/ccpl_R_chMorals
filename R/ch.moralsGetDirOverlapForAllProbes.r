#' A function to calculates the directional overlap for all probes.
#'
#' This function calculates the directional overlap for all probes and ensures every combination is present in both directions (e.g., itemA vs itemB and itemB vs itemA). Thus, there will be duplicate combinations with oppoisite directional overlaps.
#' @param data a dataframe containing overlap data for probe combinations.
#' @param probe1Col a string the specifies the column name in "data" that contains the first probe item.
#' @param probe2Col a string the specifies the column name in "data" that contains the second probe item.
#' @param overlapCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param directionCol a string that specifies the name of the column in "data" that contains the direction column that indications the direction of the overlap.
#' @return the dataframe (data), doubled in size because it adds the reverse order of the probes, with the directional overlap column (dirOverlap).
#' @export
#' @examples ch.moralsGetDirOverlapForAllProbes (overlapData, "IA1", "IB1", "overlap","direction")

ch.moralsGetDirOverlapForAllProbes <- function (data, probe1Col, probe2Col, overlapCol, directionCol) {

  data$dirOverlap <- abs(data[[overlapCol]]-1)*data[[directionCol]]
  data$probeOrder <- 'original'

  ####### In the following, I will create a stripchart of the overlaps for each item
  #  to do so, I need to get a directional overlap for each combination of items
  #  and then take the average for each item/quantity to determine it's value compared
  #  to the rest of the items in the list.
  #########
  #	 I use the original overlap dataset and then duplicate the probes in reverse order to get the
  #  directional overlap for every combination of probe1.
  ##########
  tmp <- data
  tmp$probeOrder <- 'reverse'
  tmp$tmpProbe1 <- tmp[[probe1Col]]
  tmp[[probe1Col]] <- tmp[[probe2Col]]
  tmp[[probe2Col]] <- tmp$tmpProbe1
  tmp$dirOverlap <- abs(tmp[[overlapCol]]-1)*(-1*tmp[[directionCol]])
  tmp$tmpProbe1 <- NULL

  uniqueDOdataPlusReversals <-rbind(data,tmp)

  outData <- unique(uniqueDOdataPlusReversals)
  return(outData)
}
