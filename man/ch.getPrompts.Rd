% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.getPrompts.r
\name{ch.getPrompts}
\alias{ch.getPrompts}
\title{A function to extract the unique prompts from a choice datafile.}
\usage{
ch.getPrompts(data, item1cols, item2cols)
}
\arguments{
\item{data}{A datafrane containing the raw data from a choice experiment or set of experiments.}

\item{item1cols}{a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 1.}

\item{item2cols}{a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 2.}
}
\value{
A dataframe containing the the unique prompts from a choice datafile
}
\description{
A function to extract the unique prompts from a choice datafile.
}
\examples{
ch.getPrompts (df.myInputData, c("Item1", "Item2"), c("Item3"))
}
\keyword{choice}
\keyword{extract}
\keyword{file}
\keyword{prompt}
