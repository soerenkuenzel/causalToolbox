################################################################################
######### Classes ##############################################################
node <- setClass("node")

rnode <- setClass("rnode",                           # regrular nodes NOT final
                  contains = "node",
                  slots = list(splitFeat = "numeric",
                               splitVal  = "numeric",
                               node.left = "node",
                               node.right = "node")
)


leaf <- setClass("leaf",                             # regrular nodes NOT final
                 contains = "node",
                 slots = list(feature.I = "data.frame",
                              yobs.I = "numeric"
                 )
)

#setOldClass("ranger")
htree <- setClass("htree",
                  slots = list(feature.train.J = "data.frame",
                               yobs.train.J = "numeric",
                               feature.train.I = "data.frame",
                               yobs.train.I = "numeric", 
                               StartingNode = "node")
)


################################################################################
######### Creators #############################################################
htree <- function(feat.J, yobs.J, 
                  feat.I, yobs.I, 
                  mtry, minleafsize.J, minleafsize.I){
  # I is used for averaging -- J is used for the splitting!
  feat.J <- as.data.frame(feat.J)
  feat.I <- as.data.frame(feat.I)
  if(is.null(mtry)) mtry <- ceiling(ncol(feat.I)/3)
  
  StartNode <- createNode(feat.J, yobs.J, 
                          feat.I, yobs.I,
                          mtry, minleafsize.I, minleafsize.J)
  invisible(
    new("htree",
        feature.train.J = feat.J,
        yobs.train.J = yobs.J,
        feature.train.I = feat.I,
        yobs.train.I = yobs.I, 
        StartingNode = StartNode)
  )
}

mycart <-  function(feat, yobs, mtry = NULL, minleafsize = 5){
  invisible(htree(feat, yobs, 
                  feat, yobs, 
                  mtry, minleafsize.J = minleafsize, minleafsize.I = 1)
  )
}

htreeEqualSplit <-  function(feat, yobs, mtry = NULL, minleafsize = 5){
  nobs <- length(yobs)
  neworder <- sample(1:nobs, nobs)
  feat <- feat[neworder, ]
  yobs <- yobs[neworder]
  nobs.2 <- round(nobs/2)
  
  invisible(
    htree(feat[1:nobs.2, ],        yobs[1:nobs.2], 
          feat[(nobs.2+1):nobs, ], yobs[(nobs.2+1):nobs], 
          mtry, minleafsize.J = minleafsize, minleafsize.I = 1)
  )
}

FindBestSplit.CT <- function(feat.J, yobs.J, 
                             feat.I,
                             feat.totry, 
                             minleafsize.I, minleafsize.J){
  mtry <- length(feat.totry)
  mu.bar.square.J.best <- rep(-Inf, mtry)
  # mse.combined.best <- Inf
  val.best <- rep(NA, mtry)
  feat.best <- rep(NA, mtry)
  count.times.best <- rep(0, mtry)
  for(i in 1:mtry){
    thisfeat <- feat.totry[i]
    #     # Potential SpeedUP:
    #     iorder <- order(feat.J[ , thisfeat])
    #     feati.J.sort <- feat.J[iorder, thisfeat]
    #     tr.J.sort <- tr.J[iorder]
    #     yobs.J.sort <- yobs.J[iorder]
    #     
    sv.before <- min(feat.J[ , thisfeat])
    if(length(unique(feat.J[ ,thisfeat])) == 1) next()
    for(sv in sort(unique(feat.J[ ,thisfeat]))[-1]){
      left.J <- feat.J[ ,thisfeat] < sv
      left.I <- feat.I[ ,thisfeat] < sv
      
      nleft.J  <- sum( left.J)
      nright.J <- sum(!left.J)
      
      if(min(sum(left.J), sum(!left.J)) < minleafsize.J |    # Check leaf size at least minleafsize.J  
         min(sum(left.I), sum(!left.I)) < minleafsize.I){
        next()
      }
      
      mu.bar.left.J  <- mean(yobs.J[ left.J])
      mu.bar.right.J <- mean(yobs.J[!left.J])
      
      mu.bar.square.J <- nleft.J * mu.bar.left.J^2 + nright.J * mu.bar.right.J^2
      # mu.bar.square.J <- nleft.J * mu.bar.left.J^2 + nright.J * mu.bar.right.J^2
      # mu.bar.square.J <- mu.bar.left.J + mu.bar.right.J
      
      
      
      if(mu.bar.square.J > mu.bar.square.J.best[i]){
        mu.bar.square.J.best[i] <- mu.bar.square.J
        feat.best[i] <- thisfeat
        val.best[i] <- runif(1, sv.before, sv)
        count.times.best[i] <- 1
      }else{
        if(mu.bar.square.J == mu.bar.square.J.best[i]){    # if we are as good as the best split
          count.times.best[i] <- count.times.best[i] + 1
          if(runif(1, 0, count.times.best[i]) <= 1){      # then only update with probability 1/nseen
            mu.bar.square.J.best[i] <- mu.bar.square.J
            feat.best[i] <- thisfeat
            val.best[i] <- runif(1, sv.before, sv)
          }
        }
      }
      sv.before <- sv
      
# Equivalent statement in terms of MSE:
#       mse.left  <- mean((yobs.J[left.J]  - mean(yobs.J[left.J]))^2)
#       mse.right <- mean((yobs.J[!left.J] - mean(yobs.J[!left.J]))^2)
#       
#       mse.combined <- nleft.J * mse.left + nright.J * mse.right
#       
#       if(mse.combined < mse.combined.best){
#         mse.combined.best <- mse.combined
#         feat.best <- i
#         val.best <- sv
#       }

    }
  }
  bestval <- max(mu.bar.square.J.best)
  
  thesebest <- which(mu.bar.square.J.best == bestval)
  if(sum(count.times.best) > 0){
    elements_to_sample <- rep(thesebest, count.times.best[thesebest])
    thisbest <- ifelse(length(elements_to_sample) == 1, 
                       elements_to_sample, 
                       sample(elements_to_sample, 1))
  }else{
    return(list("feat.best" = NA, "val.best" = NA))
  }
  return(list("feat.best" = feat.best[thisbest], "val.best" = val.best[thisbest]))
}

createNode <- function(feat.J, yobs.J, 
                       feat.I, yobs.I,
                       mtry, minleafsize.I, minleafsize.J){
  nfeat <- ncol(feat.J)
  feat.totry <- sample(1:nfeat, mtry)

  
  list2env(                                   # sets feat.best and val.best
    FindBestSplit.CT(feat.J, yobs.J, 
                     feat.I,
                     feat.totry, 
                     minleafsize.I, minleafsize.J), 
    environment())  # sets feat.best & val.best 
  if(is.na(val.best)){          # create a leaf
    return(
      new("leaf", 
          feature.I = feat.I,
          yobs.I    = yobs.I)
    )
  }else{                        # create a regular node
    left.J <- feat.J[ , feat.best] < val.best
    left.I <- feat.I[ , feat.best] < val.best
    
    node.left <- createNode(feat.J = feat.J[left.J, ], yobs.J = yobs.J[left.J], 
                            feat.I = feat.I[left.I, ], yobs.I = yobs.I[left.I],
                            mtry, minleafsize.I, minleafsize.J)
    node.right <- createNode(feat.J[!left.J, ], yobs.J[!left.J], 
                             feat.I[!left.I, ], yobs.I[!left.I],
                             mtry, minleafsize.I, minleafsize.J)
    
    new("rnode",
        splitFeat  = feat.best,
        splitVal   = val.best,
        node.left  = node.left,
        node.right = node.right
    )
  }
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
          signature = "leaf",
          definition = function(theObject, feature.new)
          {
            return(
              mean(theObject@yobs.I)
            )
          }
)

setMethod(f = "predict",
          signature = "rnode",
          definition = function(theObject, feature.new)
          {
            ind.left <- feature.new[ , theObject@splitFeat] < theObject@splitVal
            
            prediction <- rep(NA, nrow(feature.new))
            if(sum(ind.left) > 0 ){
              prediction[ind.left] <-  predict(theObject@node.left, feature.new[ ind.left, ])
            }
            if(sum(!ind.left) > 0 ){
              prediction[!ind.left] <- predict(theObject@node.right, feature.new[!ind.left, ])
            }
            return(prediction)
          }
)

setMethod(f = "predict",
          signature = "htree",
          definition = function(theObject, feature.new)
          {
            return(predict(theObject@StartingNode, feature.new))
          }
)

################################################################################
#### special methods ###########################################################
setGeneric(name="ShowTree",
           def=function(theObject)
           {
             standardGeneric("ShowTree")
           }
)

setGeneric(name="printnode",
           def=function(theObject)
           {
             standardGeneric("printnode")
           }
)

setMethod(f = "printnode",
          signature = "leaf",
          definition = function(theObject)
          {
            print(paste("This is a leaf with CATE:", predict(theObject)))
          }
)

setMethod(f = "printnode",
          signature = "rnode",
          definition = function(theObject)
          {
            print(paste("This is a regular node with Split feat:", theObject@splitFeat,
                        ", Split Val:", theObject@splitVal))
            
            printnode(theObject@node.left)
            printnode(theObject@node.right)
          }
)

setMethod(f = "ShowTree",
          signature = "htree",
          definition = function(theObject)
          {
            printnode(theObject@StartingNode)
          }
)



################################################################################
################################################################################
################################################################################
# Playground ###################################################################
################################################################################
if(FALSE){
  feat <- iris[,-1]
  feat$Species <- as.numeric(feat$Species)
  yobs <- iris[,1]
  
  feat.I <- feat
  feat.J <- feat
  yobs.I <- yobs
  yobs.J <- yobs
  
  mtry <- 3
  minleafsize.I<- 5
  minleafsize.J <- 5
  
  for(i in 1:2){
    thisTree <- htree(feat.J, yobs.J, 
                      feat.I, yobs.I, 
                      mtry= 4, 
                      minleafsize.J = 1, minleafsize.I = 5)
    print(mean((yobs - predict(thisTree, feat.J))^2))
  }
  # problem: This should be the same!
  
  
  
  
  
  thisCART <- mycart(feat, yobs, mtry = 4, minleafsize = 2)
  mean((yobs - predict(thisCART, feat.J))^2)
  
  
  htreeE <- htreeEqualSplit(feat, yobs, mtry = NULL, minleafsize = 5)
  mean((yobs - predict(htreeE, feat.J))^2)
  
  
  library(randomForest)
  mean((yobs - randomForest(feat, yobs)$predicted)^2)
  
  
  
  
  ##############################################################################
  # Mini Tree:
  feat <- iris[,-1]
  feat$Species <- as.numeric(feat$Species)
  yobs <- iris[,1]
  
  those <- 1:3 #c(1,56,150)
  (feat.I <- feat[those,])
  feat.J <- feat[those,]
  yobs.I <- yobs[those]
  yobs.J <- yobs[those]
  
  minleafsize.I<- 1
  minleafsize.J <- 1
  
  for(i in 1:10){
    set.seed(i)
    thisTree <- htree(feat.J, yobs.J, 
                      feat.I, yobs.I, 
                      mtry= 4, 
                      minleafsize.J = 1, minleafsize.I = 1)
    print(mean((yobs.J - predict(thisTree, feat.J))^2))
  }
  ShowTree(thisTree)
  
  
  set.seed(2)
  thisTree <- htree(feat.J, yobs.J, 
                    feat.I, yobs.I, 
                    mtry= 4, 
                    minleafsize.J = 1, minleafsize.I = 1)
  print(mean((yobs.J - predict(thisTree, feat.J))^2))
  ShowTree(thisTree)
  # this is a mistake and cannot happen (!) we say minleafsize = 1, so why is it
  # not growing a full tree?
  
  # problem: This should be the same! NOW IT WORKS
  
}