\name{flowMeans}
\alias{flowMeans}
\title{flowMeans}

\description{
Finds a good fit to the data using k-means clustering algorithm. Then
merges the adjacent dense spherical clusters to find non-spherical
clusters.
}
\usage{
flowMeans(x, varNames=NULL, MaxN = NA, NumC = NA, iter.max = 50, nstart = 10,
Mahalanobis = TRUE, Standardize = TRUE, Update = "Mahalanobis", OrthagonalResiduals=TRUE,
MaxCovN=NA, MaxKernN=NA, addNoise=TRUE)
}
\arguments{
\item{x}{A matrix, data frame of observations, or object of class \code{flowFrame}.  Rows correspond to observations and columns correspond to variables.}
\item{varNames}{A character vector specifying the variables (columns) to be included in clustering.  When it is left unspecified, all the variables will be used.}
  \item{MaxN}{
    Maximum number of clusters. If set to NA (default) the value will be
    estimated automatically.
}
  \item{NumC}{
    Number of clusters. If set to NA (default) the value will be
    estimated automatically.
}
  \item{iter.max}{
       The maximum number of iterations allowed.
}
  \item{nstart}{
       The number of random sets used for initialization.
}
  \item{Mahalanobis}{
       Boolean value. If TRUE (default) mahalanobis distance will be
       used. Otherwised, euclidean distance will be used. 
}
  \item{Standardize}{
    Boolean value. If TRUE (default) the data will be transformed to the
    [0,1] interval. 
}
  \item{Update}{
    String value. If set to "Mahalanobis" the distance function will be
    updated at each merging iteration with recalculating mahalanobis
    distances. If set to "Mean" the distance matrix will be updated
    after each merging step with averaging. If set to "None" the
    distance matrix will not be updated. 
 }
  \item{MaxCovN}{
    Maximum number of points, used for calculating the covariance. If set
    to NA (default), all the points will be used.)
}
  \item{MaxKernN}{
    Maximum number of points, used for counting the modes using kernel
    density estimation. If set to NA (default), all the points will be used.)
}
\item{addNoise}{
  Boolean value. Determines if uniform noise must be added to the data
  to prevent singularity issues or not. 
}
  \item{OrthagonalResiduals}{
    Boolean value, indicates if the residuals must be transformed to
  orthagonal distance or not.
    }

}

\details{
  If Mahalanobis distance is not used (i.e., Mahalanobis=FALSE) then the
  Update value cannot be set to Mahalanobis (i.e., Update="Mahalanobis")
  
 }
\value{
  \item{Label}{
     A vector of integers indicating the cluster to which each point is allocated. 
   }
  \item{Labels}{
     A list of vectors of integers indicating the cluster to which each
     point is allocated at each merging iteration. 
   }
  \item{Mats}{
     A list of distance matrixes between clusters at every merging iteration.
   }

   \item{MaxN}{
     Maximum number of clusters
   }
   \item{Mins}{
     A vector of integers indicating the distance between the two
     clusters chosen to be merged at every iteration.
   }
   \item{MinIndex}{
     Index of the merging step that produced the final results.
   }
   \item{Line1}{
     First regression line used for finding the changepoint for stopping
     the merging process.
     }
   \item{Line2}{
     Second regression line used for finding the changepoint for stopping
     the merging process.
   }
  }
  
\author{
  Nima Aghaeepour
}


\examples{
library(flowMeans)
data(x)
res <- flowMeans(x, c("FL1.H", "FL2.H", "FL3.H", "FL4.H"), MaxN=10)
plot(x[,c(3,4)], res, c("FL1.H", "FL2.H"))
}

\keyword{ nonparametric }
\keyword{ cluster }
\keyword{ multivariate }
