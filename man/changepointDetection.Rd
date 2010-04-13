\name{changepointDetection}
\alias{changepointDetection}
\title{
Change-Point Detection}
\description{
Fits a two-component piecewise linear regression to the minimum distance
between merged clusters vs the number of clusters for a list of merged cluster solutions.
}
\usage{
changepointDetection(vect, OrthagonalResiduals = TRUE, PlotFlag = FALSE)
}

\arguments{
  \item{vect}{
    A vector of minimum distances between clusters chosen to be merged
    at each iteration.
  }
  \item{OrthagonalResiduals}{
    Boolean value, indicates if the residuals must be transformed to
  orthagonal distance or not.
    }
  \item{PlotFlag}{
    Boolean value, indicating if the regression lines must be
    visualized. 
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
     \item{MinIndex}{
     Index of the merging step that produced the final results.
   }
   \item{l1}{
     First regression line used for finding the changepoint for stopping
     the merging process.
     }
   \item{l2}{
     Second regression line used for finding the changepoint for stopping
     the merging process.
   }
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Nima Aghaeepour
}
%\note{
%%  ~~further notes~~
%}


%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{

library(flowMeans)
data(x)
res <- flowMeans(x, c("FL1.H", "FL2.H", "FL3.H", "FL4.H"), MaxN=10)
ft<-changepointDetection(res@Mins)
plot(res@Mins)
abline(ft$l1)
abline(ft$l2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ piecewise regression }

