#Determines if two cluster solutions are the same or not.
inSameCluster <-
function(I,J, MergedClusters){
  for (i in 1:length(MergedClusters))
    if (length(which(MergedClusters[[i]]==I))+length(which(MergedClusters[[i]]==J))==2)
      return (TRUE);
  return(FALSE);
}

