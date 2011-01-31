#Calculates the distance matrix between pairs of cluters. Cluster memberships are provided in Label and Mahalanobis determines if the distance function is mahalanobis or euclidean.
distanceMatrix <-
  function(x, Label, Mahalanobis, MaxCovN){
    n <- max(Label);
    mat=matrix(0,n,n)
    for (i in 1:n){
      for (j in 1:n){
        Li <- x[which(Label==i),]
        Lj <- x[which(Label==j),]
        if (!is.vector(Li)){
          if (Mahalanobis){
            MaxCovNumber<-min(MaxCovN, length(Li[,1]));
            mat[i,j]=mean(mahalanobis(Li, colMeans(Lj), cov(Li[1:MaxCovNumber,])))
          }
          else
            mat[i,j]=sqrt(sum(colMeans(Li)-colMeans(Lj))^2)
        }
        if (is.vector(Li)){
          if (Mahalanobis){
            MaxCovNumber<-min(MaxCovN, length(Li));
            ##mat[i,j]=mean(mahalanobis(Li, mean(Lj), cov(Li[1:MaxCovNumber])))
            mat[i,j]=sum(Li-mean(Lj))^2/sd(Li[1:MaxCovNumber])
          }
          else
            mat[i,j]=sqrt((mean(Li)-mean(Lj))^2)
        }
      }
    }
    return(mat);
  }

