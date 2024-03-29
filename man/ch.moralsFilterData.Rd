% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.moralsFilterData.r
\name{ch.moralsFilterData}
\alias{ch.moralsFilterData}
\title{A function to filter the morals data for analysis}
\usage{
ch.moralsFilterData(
  data,
  snCol,
  RTcol,
  overlapRoundCol,
  aveRTcol,
  correctCol,
  correctVals = c(TRUE, FALSE),
  item1cols = c("Item1"),
  item2cols = c("Item2"),
  params
)
}
\arguments{
\item{data}{the morals dataframe after running through ch.moralsDataPrep().}

\item{snCol}{a string that specifies the name of the column in "data" that contains the subject number.}

\item{RTcol}{a string that specifies the name of the column in "data" that contains the RT for each trial.}

\item{overlapRoundCol}{a string that specifies the name of the column in "data" that contains the rounded overlaps for the item in each trial. Column occured in ch.moralsDataPrep().}

\item{aveRTcol}{a string that specifies the name of the column in "data" that contains the average RT by subject. Column created in ch.moralsDataPrep().}

\item{correctCol}{a string that specifies the name of the new column that will contains a "1" if the participant chose the item with the greatest value distribution and a "0" if they did not.}

\item{correctVals}{a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")}

\item{item1cols}{a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 1.}

\item{item2cols}{a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 2.}

\item{params}{a list of parameters that are read in using "ch.readMoralsDBfile.r."}
}
\value{
a dataframe of filtered data.  It also writes the data to "analysisReadyData.txt," which will be used by other functions.
}
\description{
This function filters the morals data for analysis by removing RT outliers and bad subjects based on RT and p(hit). The thresholds for removal are set in the moralsDBfile that is read and stored in a parameter list.
}
\examples{
ch.moralsFilterData (data=moralsData, "sn", "RT", "overlapRound", "aveRT", "avePred", params=parameters)
}
\keyword{data}
\keyword{filter}
\keyword{morals}
