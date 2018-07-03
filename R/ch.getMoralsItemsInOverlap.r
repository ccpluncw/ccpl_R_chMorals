#' A function to get the Items in a single Overlap
#'
#' This function outputs a dataframe containing the items in a single overlap.
#' @param data a dataframe.
#' @param overlapCol a string specifying the name of the column in the dataframe containing the overlap variable.
#' @param itemCols a vector containing strings that specify the names of the columns in the dataframe containing the item variables.
#' @param overlap a number specifying the overlap that you want to find the items for.
#' @return a dataframe containing the items that overlapCol == overlap
#' @export
#' @examples ch.getItemsInOverlap (df, "overlapRound", c("Item1", "Item2"),0.1)

ch.getMoralsItemsInOverlap <- function(data, overlapCol, itemCols, overlap) {

    data1 <- unique(data[data[[overlapCol]] == overlap,c(itemCols,overlapCol)])
    data2 <- data1[!duplicated(t(apply(data1, 1, sort))),]
    return (data2)
}

#' A function to get an ordered, non-duplicate list of the combinations of Items for each overlap
#'
#' This function outputs a list containing: 1) a dataframe containing an ordered, non-duplicate combinations of Items for each overlap, and 2) a dataframe containing the number of non-duplicate combinations of items for each overlap.
#' @param data a dataframe.
#' @param overlapCol a string specifying the name of the column in the dataframe containing the overlap variable.
#' @param itemCols a vector containing strings that specify the names of the columns in the dataframe containing the item variables.
#' @return a list containing: 1) a dataframe containing an ordered, non-duplicate combinations of Items for each overlap, and 2) a dataframe containing the number of non-duplicate combinations of items for each overlap.
#' @export
#' @examples ch.getItemsInAllOverlaps (df, "overlapRound", c("Item1", "Item2"))

ch.getMoralsItemsInAllOverlaps <- function(data, overlapCol, itemCols) {
  library(chutils)

    data1 <- unique(data[,c(itemCols,overlapCol)])
    data2 <- data1[!duplicated(t(apply(data1, 1, sort))),]
    data2 <- data2[order(data2[[overlapCol]]),]
    data3 <- ch.summariseBy (data2, overlapCol, overlapCol, "N", length)
    return (list(overlapItemN = data3, overlapItemSummary = data2))
}
