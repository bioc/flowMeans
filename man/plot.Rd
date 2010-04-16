\name{plot}
\alias{plot}
\alias{plot,ANY,Populations}
\alias{plot,ANY,Populations-method}
\alias{plot,flowFrame,Populations}
\alias{plot,flowFrame,Populations-method}

\docType{methods}

\title{Scatterplot of Clustering Results} 

\description{
This method generates scatterplot revealing the cluster assignment.
}

\usage{
\S4method{plot}{ANY,Populations}(x, y, varNames=NULL, ...)
\S4method{plot}{flowFrame,Populations}(x, y, varNames=NULL, ...)
}

\arguments{
\item{x}{A matrix, data frame of observations, or object of class \code{flowFrame}. This is the object on which \code{flowClust} was performed.}
\item{y}{Object returned from \code{\link{flowMeans}}.}
\item{varNames}{A character vector specifying the variables (columns) to be included in the plot.  When it is left unspecified, all the variables will be used.}
\item{...}{Extra parameters that will be passed to the generic plot function}
}


\author{
Nima Aghaeepour <\email{naghaeep@bccrc.ca}>
}


\seealso{
\code{\link{flowMeans}}
}

\examples{
library(flowMeans)
data(x)
res <- flowMeans(x, c("FL1.H", "FL2.H", "FL3.H", "FL4.H"), MaxN=10)
plot(x, res, c("FL1.H", "FL2.H"))
}

\keyword{graphs}





