#Replaces the label of the outlier cells with the label of the closest cluster.
OutlierRemover <-
function(x, Label, MaxCovN){
  n <- max(Label);
  mat=matrix(0,n,n)
  L <- list();
  Cov <- list();
  for (i in 1:n){
    L[[i]] <- x[which(Label==i),]
    MaxCovN<-min(MaxCovN, length(L[[i]][,1]));
    Cov[[i]] <- cov(L[[i]][1:MaxCovN,])
  }
  for (i in 1:length(Label)){
    if ((Label[i]==0) || is.na(Label[i]) || is.nan(Label[[i]])){
      Label[i] <- 1;
      Min <- mean(mahalanobis(L[[1]], as.matrix(x[i,]), Cov[[1]]))      
      for (j in 2:n){
        temp <-  mean(mahalanobis(L[[j]], as.matrix(x[i,]), Cov[[j]]))
        if (temp<Min){
          Label[i] <- j;
          Min <- temp
        }
      }
    }
  }
  return(Label);
}

