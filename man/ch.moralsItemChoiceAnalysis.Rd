% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.moralsItemChoiceAnalysis.r
\name{ch.moralsItemChoiceAnalysis}
\alias{ch.moralsItemChoiceAnalysis}
\title{A function to graph the response choice pattern per item in the morals experiment}
\usage{
ch.moralsItemChoiceAnalysis(
  data,
  item1Col,
  item2Col,
  overlapRoundCol,
  dirOverlapCol,
  respChoiceCol,
  respChoiceVal = c("Item1", "Item2"),
  params,
  saveFigures = T,
  comparisonItemName = NULL
)
}
\arguments{
\item{data}{morals dataframe.}

\item{item1Col}{a string that specifies the name of the column in "data" that contains one of the items in the morals task.}

\item{item2Col}{a string that specifies the name of the column in "data" that contains the comparison item in the morals task.}

\item{overlapRoundCol}{a string that specifies the name of the column in "data" that contains the overlap column.}

\item{dirOverlapCol}{a string that specifies the name of the column in "data" that contains the directional Overlap for the item in each trial.}

\item{respChoiceCol}{a string that specifies the name of the column in "data" that contains the the participant's response to the prompt - yes take action or no take no action.}

\item{respChoiceVal}{a vector of two values that specifies the choose Item1 option ("yes" take action in many morals experimants) value (index 1) and the choose Item1 option ("no" take no action in many morals experimants) value (index 2).}

\item{params}{a list of parameters that are read in using "ch.readMoralsDBfile.r."}

\item{saveFigures}{a boolean to state whether to save the figures}

\item{comparisonItemName}{a string that states the exact item that you want to compare to all the other items.  If this parameter is used, then only one item will be compared to all the others.  Otherwise, all items will be compared to each other. Default comparisonItemName = NULL.}

\item{directionCol}{a string that specifies the name of the column in "data" that contains the direction of the overlap for each trial.}
}
\value{
the dataset used to create the graphs.  Because this dataset doubles the actual data (all A-B item combinations are duplicated to be B-A combinations as well), do not use it for analysis unless you keep this in mind. The duplication is needed because a graph showing the responses to Item A, needs to have all Item A responses, regardless of order.  It is expected that the graph showing responses to Item B will include some of Item A when there is an Item A-Item B combination.
}
\description{
This function graphs the response choice pattern per item in the morals experiment.
}
\examples{
ch.moralsItemChoiceAnalysis (analysisReadyData.gp, "Item1", "Item2", "overlapRound", "dirOverlap","keyDef", respChoiceVal = c("Yes", "No"), params, printFigures = T)
}
\keyword{analysis}
\keyword{choice}
\keyword{item}
\keyword{morals}
