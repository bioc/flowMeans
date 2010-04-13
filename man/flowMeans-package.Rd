\name{flowMeans-package}
\alias{flowMeans-package}
\alias{flowMeans-package}
\docType{package}
\title{flowMeans Package}
\description{
Non-parametric Flow Cytometry Data Gating
}
\details{
\tabular{ll}{
Package: \tab flowMeans\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-03-02\cr
License: \tab Artistic-2.0 or newer\cr
LazyLoad: \tab yes\cr
}
}
\author{
  Nima Aghaeepour <naghaeep@bccrc.ca>
}


\keyword{ nonparametric }
\keyword{ cluster }
\keyword{ multivariate }

\examples{
library(flowMeans)
data(x)
res <- flowMeans(x, c("FL1.H", "FL2.H", "FL3.H", "FL4.H"), MaxN=10)
plot(x[,c(3,4)], res, c("FL1.H", "FL2.H"))
}
