#' A function to create a vector of the column names containing the items in the morals experiment
#'
#' This function creates a vector of string names with the colBaseName followed by numbers to specify the column names
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @param colBaseName a string with the base of the column name containing the items. DEfAULT = "Item"
#' @keywords morals item column names
#' @return the vector of string names
#' @export
#' @examples ch.getMoralsItemColumnNames (params)

ch.getMoralsItemColumnNames <- function (params, colBaseName = "Item") {
	groupA <- seq(params$minGroupAnum, params$maxGroupAnum, 1)
	groupB <- seq(params$minGroupBnum, params$maxGroupBnum, 1)

	#get all combinations that produce the same outcomes
	xCol <- NULL
	for (i in groupA) {
		xTmp <- paste(colBaseName, i, sep="")
		if (is.null(xCol)) {
			xCol <- xTmp
		} else {
		xCol <- append(xCol, xTmp)
		}
	}
	for (i in groupB) {
		xTmp <- paste(colBaseName, i, sep="")
		if (is.null(xCol)) {
			xCol <- xTmp
		} else {
		xCol <- append(xCol, xTmp)
		}
	}
  return(xCol)

}
