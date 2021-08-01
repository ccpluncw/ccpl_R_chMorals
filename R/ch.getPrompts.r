#' A function to extract the unique prompts from a choice datafile.
#'
#' @param data A datafrane containing the raw data from a choice experiment or set of experiments.
#' @param item1cols a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 1.
#' @param item2cols a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 2.
#' @keywords prompt choice file extract
#' @return A dataframe containing the the unique prompts from a choice datafile
#' @export
#' @examples ch.getPrompts (df.myInputData, c("Item1", "Item2"), c("Item3"))

ch.getPrompts <- function(data, item1cols, item2cols) {

  df.out<- unique(data[,c(item1cols, item2cols)])

  return(df.out)
}
