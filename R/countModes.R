#counts the number of modes in a vector based on significant curvature
countModes <- function(x,...){
 y<-featureSignif(x, bw=bw.nrd(x), ...)
 count<-0
 for (i in 2:length(y$curv)){
   if (y$curv[i]==1 && y$curv[i-1]==0)
     count<-count+1
 }
 return(list(density=y, NumberOfModes=count))
}
