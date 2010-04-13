#Finds the two closest clusters that have not been merged yet. MergedClusters is a list of vectors of clusters that have been merged so far. 
nextMerge <-
function(mat, MergedClusters){
  Max<-max(mat)
  Min<-Max*2
  n <- length(mat[,1]);
  I<-0;
  J<-0;
  for (i in 1:n){
    for (j in 1:n){
      if (i==j)
        next;
      if ((mat[i,j]+mat[j,i])<Min){
        if (inSameCluster(i, j, MergedClusters))
          next;
        Min=mat[i,j]+mat[j,i];
        I <- i;
        J <- j;
      }
    }
  }
  return(list(I=I, J=J, Min=Min));
}

