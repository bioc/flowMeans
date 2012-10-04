#The main clustering framework's procedure. See the Rd document for details.


flowMeans <-
function(x, varNames=NULL, MaxN=NA, NumC=NA, iter.max=50, nstart=10, Mahalanobis=TRUE, Standardize=TRUE, Update='Mahalanobis', OrthagonalResiduals=TRUE, MaxCovN=NA, MaxKernN=NA, addNoise=TRUE){

  if(is(x,"flowFrame"))
  {
      if(length(varNames)==0)
      {
          y<-exprs(x)     # y<-exprs(x)[,x@parameters[[1]]]
          varNames<-colnames(y)     # varNames<-as.character(x@parameters[[1]])
      }
      else
      {
          y<-as.matrix(exprs(x)[,varNames])
      }
  }
  else if(is(x,"matrix"))
  {
      if(length(varNames)==0)
      {
          y<-x
          if (length(colnames(x))==0) varNames <- "Not Available"  else varNames <- colnames(x)
      }
      else
      {
          y<-as.matrix(x[,varNames])
      }
  }
  else if(is(x,"data.frame"))
  {
      if(length(varNames)==0)
      {
          y<-as.matrix(x)
          varNames<-colnames(x)
      }
      else
      {
          y<-as.matrix(x[,varNames])
      }
  }
  else if(is(x,"vector"))
  {
      y<-matrix(x)
      if(length(varNames)==0) varNames<-"Not Available"
  }
  else
  {
    stop(paste("Object ", as.character(x)," is not of class flowFrame / matrix / data frame!"))
  }

  x<-y
  
  if (length(is.finite(x))!=length(x))
    stop('One or more of the values in \'x\' are not finite (i.e., are NaN, NA, Inf, or -Inf');

  ##if (length(x[1,])<2)
    ##stop('flowMeans only works on multidimentional data (2 or more variables are required');

  if (addNoise){
    set.seed(546)
    nfactor=0.05
    x = x + runif(length(x), nfactor*-1, nfactor)
  }
  
  if (Standardize){
    for (i in 1:length(x[1,])){
      x[,i] <- x[,i] - min(x[,i]);
      x[,i] <- x[,i] / max(x[,i]);      
    }
  }

  if (Update == 'Mahalanobis'){
    if (!Mahalanobis)
      Update='Mean'
  }
  
  if (is.na(MaxKernN)){
    MaxKernN <- length(x[,1]);
  }
  if (is.na(MaxCovN)){
    MaxCovN <- length(x[,1]);
  }

  if (is.na(MaxN)){
    MaxN <- 0;
    for (i in 1:length(x[1,]))
      MaxN<- (MaxN + countModes(x[1:MaxKernN,i])$NumberOfModes);
    MaxN <- max(MaxN,3)
  }
  
  km<-kmeans(x,MaxN, iter.max=iter.max, nstart=nstart)

  Label <- km$cluster;
  
  mat<-distanceMatrix(x, Label, Mahalanobis, MaxCovN);
  
  Max<-max(mat)
  Mins <- vector();
  Mats<-list();
  N<-max(Label)
  Labels<-list();
  Mats[[1]] <- mat;
  Labels[[1]] <- Label;
  MergedClusters <- list();
  ListOfLabels <- c(1:MaxN);
  for (i in 1:MaxN)
    MergedClusters[[i]] <- c(i);
  while(max(Label)>1){
    #print(max(Label));
    Min<-Max*2
    I<-0;
    J<-0;
    TI<-0;
    TJ<-0;
    if (Update=='None'){
      temp <- nextMerge(mat, MergedClusters);
      Min <- temp$Min;
      TI <- temp$I;
      TJ <- temp$J;
      MergedClusters <- updateMergedClusters(TI,TJ,MergedClusters);
      I <- ListOfLabels[TI];
      J <- ListOfLabels[TJ];
      #print(c(TI, TJ))
      #print(c(I, J))
      #print(MergedClusters);
    }
    else{
      for (i in 1:N){
        for (j in 1:i){
          if (i==j)
            next;
          ij <- min(mat[i,j],mat[j,i])
          if (ij<Min){
            Min=ij
            I <- i;
            J <- j;
          }
        }
      }
      TI=I;
      TJ=J
    }
    Mins[MaxN-N+1] <- Min;
    temp <- MergeLabels(Label,ListOfLabels, I, J, TI, TJ);
    Label <- temp$Label
    ListOfLabels <- temp$ListOfLabels
    N<-max(Label)
    #print(Label[1:30])
    #print(ListOfLabels)
    
    if (Update=='Mahalanobis')
      mat=distanceMatrix(x, Label, Mahalanobis, MaxCovN);
    if (Update=='Mean')
      mat=MergeMatrix(mat, I, J);

    Labels[[MaxN-N+1]] <- Label;
    Mats[[MaxN-N+1]] <- mat;
  }
  Mins[MaxN-N+1] <- Min;

  temp <- changepointDetection(Mins, OrthagonalResiduals=OrthagonalResiduals);
  Line1<-temp$l1;
  Line2<-temp$l2;
  MinIndex <- MaxN-temp$MinIndex;
  Label <- Labels[[MaxN-MinIndex+1]]

  if (!is.na(NumC)){
    MinIndex <- NumC
    Label <- Labels[[MaxN-MinIndex+1]]
  }

  
  
  return(new("Populations", Label=Label, Labels=Labels, MinIndex=MinIndex, MaxN=MaxN, Mats=Mats, Mins=Mins, Line1=Line1, Line2=Line2))
}
