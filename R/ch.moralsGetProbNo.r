#' A function to assess the participants bias to say no as a function of "x"
#'
#' This function assesses the participants bias to say no as a function of "x"
#' @param data the dataframe.
#' @param x a string the specifies the column name in "data" that contains the x variable.
#' @param yesNoCol a string the specifies the column name in "data" that contains the variable with the participant's yes/no response.
#' @param respYNVal a vector of two values that specifies the yes "take action" value (index 1) and the no "take no action" value (index 2). e.g, c("yes", "no")
#' @param summarize Do you want to collapse the data by "x"? DEFAULT = TRUE.
#' @param plotFilename the filename to save the plot (pdf) if you want it. DEFAULT = NULL (no plot saved).
#' @keywords morals probability no
#' @return an lm fit object.
#' @export
#' @examples ch.moralsGetProbNo (data=moralsData, "overlapRound", "respDef", c("Yes", "No"))

ch.moralsGetProbNo <- function (data, x, yesNoCol, yesNoVal = c("Yes", "No"), summarize = T, plotFilename = NULL, ...) {

  data$NoResp <- ifelse(data[[yesNoCol]]==yesNoVal[2],1,0)

  if(summarize) {
    overno<-ddply(data,x, summarise, propNo =mean(NoResp))
  } else {
    overno <- data
  }

  fitno <- ch.plot.lm(overno[[x]], overno$propNo, filename = plotFilename, yLabel = "p(No)", ...)

  return(fitno)
}
