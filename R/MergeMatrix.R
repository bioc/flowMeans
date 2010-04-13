#Merges the columns i and j and rows i and j of the matrix Mat (i.e., replaces the values by their average).
MergeMatrix <-
function(Mat, i, j){
  n <- length(Mat[1,]);
  if (i>j){
    temp <- i;
    i <- j;
    j <- temp;
  }
  Mat[,i] <- (Mat[,i]+Mat[,j])/2;
  Mat[i,] <- (Mat[i,]+Mat[j,])/2;
  Mat[,j] <- Mat[,n];
  Mat[j,] <- Mat[n,];
  return(Mat[1:(n-1), 1:(n-1)]);
}

