% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.moralsPlotFitsByGrps.r
\name{ch.moralsPlotFitsByGrps}
\alias{ch.moralsPlotFitsByGrps}
\title{A function to plot multiple model fits on one graph}
\usage{
ch.moralsPlotFitsByGrps(df.models, xxyCols, xCol, data, filenameID = NULL)
}
\arguments{
\item{df.models}{a models list containing: a dfIndex, created by ch.subsetDFbyGroups() and a series of model fits output by ch.getLmModel() or ch.getPhitModel();}

\item{xxyCols}{a vector of 1 or 2 strings that specifies the names of the column in "dfIndex" that contains the grouping variables (up to two).  This is required.}

\item{xCol}{a string that specifies the name of the column in "data" that contains the predictor (x) variables for the fit specified in the models.}

\item{data}{the morals dataframe after running through ch.moralsDataPrep().}

\item{filenameID}{a string that will be put on all filenames when the plots are saved. DEFAULT = NULL (no file saved)}
}
\value{
.
}
\description{
This function plots multiple model fits on one graph.
}
\examples{
ch.moralsXXYPlotFitsByGrp(df.models, "scenarioType", "agent", "overlapRound", moralsData)
}
\keyword{fit}
\keyword{group}
\keyword{model}
\keyword{morals}
\keyword{plots}
