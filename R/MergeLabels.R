#Merges the labels i and j (TI and TJ are the labels based on the initial clustering) and updates the list of remaining labels (ListOfLabels).
MergeLabels <-
function(Labels, ListOfLabels, i, j, TI, TJ){
  if (i>j){
    temp <- i;
    i <- j;
    j <- temp;
  }
  if (TI>TJ){
    temp <- TI;
    TI <- TJ;
    TJ <- temp;
  }
  Max<-max(Labels);
  ITJ=which(ListOfLabels==j)
  IMax=which(ListOfLabels==Max)
  ListOfLabels[IMax] <- ListOfLabels[TJ]
  ListOfLabels[ITJ] <- ListOfLabels[TI]
  Labels[which(Labels==j)] <- i;
  Labels[which(Labels==Max)] <- j;
  return(list(Labels=Labels, ListOfLabels=ListOfLabels));
}

