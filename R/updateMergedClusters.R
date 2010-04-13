#Updates the set of merged clusters when clusters I and J are merged. 
updateMergedClusters <-
function(I,J,MergedClusters){
  for (i in 1:length(MergedClusters)){
    if (length(which(MergedClusters[[i]]==I))==1)
      IndI=i;
    if (length(which(MergedClusters[[i]]==J))==1)
      IndJ=i;    
  }
  for (i in 1:length(MergedClusters[[IndJ]])){
    if (length(which(MergedClusters[[IndI]]==MergedClusters[[IndJ]][i]))==1)
      next;
    MergedClusters[[IndI]][length(MergedClusters[[IndI]])+1] <- MergedClusters[[IndJ]][i];
  }
  MergedClusters[[IndJ]]=NULL;
  return(MergedClusters);
}

