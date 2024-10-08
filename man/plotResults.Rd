% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrapper.R
\name{plotResults}
\alias{plotResults}
\title{plot Results}
\usage{
plotResults(
  model.setup = list(),
  output.data = data.frame(),
  plot.models = character(c()),
  plot.type = "scatter"
)
}
\arguments{
\item{model.setup}{lists of details about data, model, and site from setModels()}

\item{output.data}{dataframe of daily concentration and baseflow predictions from \code{getResults}}

\item{plot.models}{character string vector with a 'C#' model name from provided models (i.e. C1-C15). Ch1 and C13 default. Two model limit.}

\item{plot.type}{character string of either "scatter" or "timeseries" to view results. Note, 'timeseries' exports plot as a pdf in the working directory}
}
\value{
annual timeseries plot comparing predictions from two models with observations, streamflow and baseflow; C-Q scatter plots of each model
}
\description{
plotResults plots predictions from each fitted C-Q model
}
\details{
\code{plotResults}


plots C-Q scatter plots and annual timeseries with predictions from fitted C-Q models
}
\keyword{plotResults}
