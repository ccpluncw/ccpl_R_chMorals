#' A function to get the RT axis label
#'
#' This function gets the RT axis label.
#' @param transformType a string that is either "log" (transform RT to logs) or something else.
#' @param useResids boolean that specifies whether we use the residuals to a learning function as the DV.
#' @keywords morals RT axis label
#' @return a string that specifies the RT axis label.
#' @export
#' @examples ch.getMoralsRTaxisName ("log", TRUE)


ch.getMoralsRTaxisName <- function(transformType, useResids) {
  rtOut <- "RT"
  logOut <- ""
  resOut <- ""

  if (transformType == "log") {
    logOut <- "l"
  }
  if(useResids) {
    resOut <- "res"
  }

  outString <- paste(logOut, rtOut, resOut, sep = "")
  return (outString)
}
