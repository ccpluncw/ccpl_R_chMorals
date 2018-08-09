#' A function to equate the probe 1 and probe 2 strings when quantity varies and bin the quantities into categories.
#'
#' This function equates the probe 1 and probe 2 strings when quantity varies and bins the quantities into categories.
#' @param data morals dataframe.
#' @param probe1Col a string the specifies the column name in "data" that contains the first probe item.
#' @param probe2Col a string the specifies the column name in "data" that contains the second probe item.
#' @param quantValueCuts a vector of numbers that specifies the cutoff values for each quantity.  The first category is <= to the first cutoff value; each successive category (c1, c2, etc) is c1 < x <= c2; the final category is greater than the last number in the vector.
#' @return the dataframe (data) with six new columns: "p1Quant" and "p1Quant" (the quantities of probe 1 and 2 respectively); "p1" and "p2" (the singular, equalized names of probe 1 and 2 respoectively); and "p1GrpSize" and "p2GrpSize" (the group categories based on quantValueCuts for probe 1 and 2 respectively).
#' @keywords morals quantity group equate
#' @export
#' @examples ch.moralsQuantsToGrps (analysisReadyData.gp, "probe1", "probe2", c(1, 10, 40))

ch.moralsQuantsToGrps <- function(data, probe1Col, probe2Col, quantValueCuts) {

  data <- ch.moralsEquateItemStringsForNumberQuantDataset(data, probe1Col, probe2Col)
  data <- ch.numberVectorToGrps(data, "p1Quant", "p1GrpSize", quantValueCuts)
  data <- ch.numberVectorToGrps(data, "p2Quant", "p2GrpSize", quantValueCuts)

  return(data)

}
