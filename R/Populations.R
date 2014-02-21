#The Populations class and show/summary/plot methods.
setClass("Populations",
        representation(Label="vector", Labels="list", MinIndex="numeric", MaxN="numeric", Mats="list", Mins="vector", Line1="lm", Line2="lm"))

setMethod("plot", signature(x="flowFrame", y="Populations"), function(x, y, varNames=NULL, ...){
  plot(exprs(x), y, varNames, ...);
})

setMethod("plot", signature(x="ANY", y="Populations"), function(x, y, varNames=NULL, ...){
  if(is(x,"flowFrame"))
  {
      if(length(varNames)==0)
      {
          plot(data.frame(as.matrix(exprs(x))), col=y@Label, ...);
      }
      else
      {
          plot(data.frame(as.matrix(exprs(x)[,varNames])), col=y@Label, ...);
      }
  }
  else if(is(x,"matrix"))
  {
      if(length(varNames)==0)
      {
          plot(data.frame(x), col=y@Label, ...);
      }
      else
      {
          plot(data.frame(x[,varNames]), col=y@Label, ...);
      }
  }
  else if(is(x,"data.frame"))
  {
      if(length(varNames)==0)
      {
          plot(x, col=y@Label, ...);
      }
      else
      {
          plot(x[,varNames], col=y@Label, ...);
      }
  }
  else
  {
    stop(paste("Object ", as.character(x)," is not of class flowFrame / matrix / data frame!"))
  }
})

setMethod("show", signature(object="Populations"), function(object){
    cat("Object of class 'flowMeans'","\n")
    cat("This object has the following slots: \n")
    cat("Label, Labels, MinIndex, MaxN, Mats, Mins, Line1, Line2\n")
})

setMethod("summary", signature(object="Populations"), function(object,...){
 cat(sprintf("flowMeans object with %d populations:\n\n", max(object@Label)));
 for (i in 1:max(object@Label))
   cat(sprintf("%d cells (%%%.03f) in population %d.\n",
length(which(object@Label==i)), 100*length(which(object@Label==i))/length(object@Label),i));
})
