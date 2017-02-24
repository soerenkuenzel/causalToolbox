library(doParallel)
library(foreach)

################################################################################
######### Classes ##############################################################
hRandomForest <- setClass("hRandomForest",
                          slots = list(feature.train = "data.frame",
                                       yobs.train = "numeric",
                                       hTrees = "list")
)


################################################################################
######### Creators #############################################################
hRandomForest <- function(feat, yobs, mtry = NULL, ntree = 10, nthreats = 1, 
                          minleafsize = 5, verbose = FALSE, 
                          s = nrow(feat)){
  # I is the leafsize for the splitting samples
  nobs <- length(yobs)
  
  registerDoParallel(nthreats)
  HonestTrees <-
    foreach(i = 1:ntree) %dopar%{
      # sample without replacement --> bootstrap sample:
      id.bs <- sample(1:nobs, s, replace = FALSE)
      feat.bs <- feat[id.bs,]
      yobs.bs <- yobs[id.bs]
      
      if(verbose) print(paste("Done with tree", i, "out of", ntree))
      return(
        htreeEqualSplit(feat, yobs, mtry = mtry, minleafsize)
      )
    }
  
  invisible(
    new("hRandomForest",
        feature.train = feat,
        yobs.train = yobs,
        hTrees = HonestTrees)
  )
}

myRandomForest <- function(feat, yobs, mtry = NULL, ntree = 10, nthreats = 1, 
                          minleafsize = 5, verbose = FALSE){
  # I is the leafsize for the splitting samples
  nobs <- length(yobs)
  
  registerDoParallel(nthreats)
  hTrees <-
    foreach(i = 1:ntree) %dopar%{
      # sample without replacement --> bootstrap sample:
      id.bs <- sample(1:nobs, nobs, replace = TRUE)
      feat.bs <- feat[id.bs,]
      yobs.bs <- yobs[id.bs]
      
      if(verbose) print(paste("Done with tree", i, "out of", ntree))
      return(
        htree(feat.bs, yobs.bs, 
              feat.bs, yobs.bs, 
              mtry, minleafsize.J = minleafsize, minleafsize.I = minleafsize)
      )
    }
  
  invisible(
    new("hRandomForest",
        feature.train = feat,
        yobs.train = yobs,
        hTrees = hTrees)
  )
}


################################################################################
########## Methods #############################################################
if(!isGeneric("predict")){
  setGeneric(name="predict",
             def=function(theObject, feature.new)
             {
               standardGeneric("predict")
             }
  )
}
setMethod(f = "predict",
          signature = "hRandomForest",
          definition = function(theObject, feature.new)
          {
            ntree <- length(theObject@hTrees)
            indipred <- foreach(i = 1:ntree, .combine="rbind") %dopar%{
              predict(theObject@hTrees[[i]], feature.new)
            }
            
            return(apply(indipred,2,mean))
          }
)






