% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.getMoralsItemsInOverlap.r
\name{ch.getMoralsItemsInOverlap}
\alias{ch.getMoralsItemsInOverlap}
\title{A function to get the Items in a single Overlap}
\usage{
ch.getMoralsItemsInOverlap(data, overlapCol, itemCols, overlap)
}
\arguments{
\item{data}{a dataframe.}

\item{overlapCol}{a string specifying the name of the column in the dataframe containing the overlap variable.}

\item{itemCols}{a vector containing strings that specify the names of the columns in the dataframe containing the item variables.}

\item{overlap}{a number specifying the overlap that you want to find the items for.}
}
\value{
a dataframe containing the items that overlapCol == overlap
}
\description{
This function outputs a dataframe containing the items in a single overlap.
}
\examples{
ch.getItemsInOverlap (df, "overlapRound", c("Item1", "Item2"),0.1)
}
