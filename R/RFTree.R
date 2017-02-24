######################################
### Random Forest Tree Constructor ###
######################################
# Set two new "union" class
setClassUnion("RFNodeOrNULL",members=c("RFNode", "NULL"))
setClassUnion("numericOrNULL",members=c("numeric", "NULL"))

#' @name RFTree-class
#' @rdname RFTree-class
#' @exportClass RFTree
#' @description `RFTree` is the unit component in the `RF` which composes
#' `RFNode`. The tree uses recursively partitioning to determine the best
#' `splitFeature` and `splitValue` for each level, and recursively split the
#' dataset until it reaches the limitation according to `nodesize`.
#' @slot x A data frame or a matrix of all predictors.
#' @slot y A response vector.
#' @slot mtry Number of variables randomly sampled as candidates at each split.
#' The default value is set to be one third of total feature amount.
#' @slot nodesize Minimum size of terminal nodes. Setting this number larger
#' causes smaller trees to be grown (and thus take less time). The default value
#' is 5.
#' @slot maxnodes Maximum number of terminal nodes trees in the forest can have.
#' If not given, trees are grown to the maximum possible (subject to limits by
#' nodesize).
#' @slot sampleIndex A list of the index of observations that are used as
#' averaging dataset. The index are based on the original dataset `x` and `y`
#' from forest. Essentially, `x[sampleIndex]` generates the whole splitting
#' dataset.
#' @slot splitrule A splitting rule to determine the best split point among the
#' features. There are two possible split rules in the package, `variance` and
#' `maxstats`. The default is `variance` to minimize the overall MSE.
#' @slot avgfunc An averaging function to average all the input data. The input
#' of this function should be a dataframe of predictors `x` and a vector of
#' outcome `y`. The output is a scalar. The default is to take the mean of all
#' the `y`s.
#' @slot root A `RFNode` object which is the root of the tree. If the class is
#' extended, the list may contain the corresponding extended `RFNode` object.
#' @slot totalNodes	A counter of total amount of nodes in the tree.
#' @slot totalTerminalNodes A counter of total amount of terminal nodes in the
#' tree.
setClass(
  Class="RFTree",
  slots=list(
    x="data.frame",
    y="vector",
    mtry="numeric",
    nodesize="list",
    maxnodes="numeric",
    sampleIndex="list",
    splitrule="character",
    avgfunc="function",
    root="RFNodeOrNULL",
    totalNodes="numeric",
    totalTerminalNodes="numeric"
  )
)

#' `RFTree` Constructor
#' @name RFTree
#' @rdname RFTree-class
#' @description The actual storation of `RFTree` is different from its input.
#' `sampleIndex` and `nodesize` are actually two lists composing of those in
#' splitting dataset and averaging dataset. In the original random forest,
#' those are the same value. For other variant of random forest, such as honest
#' random forest, they can be set differently. The constructor can be
#' overwritten to reflect the change.
RFTree <- function(x, y, mtry=max(floor(ncol(x)/3), 1), nodesize=5,
                   maxnodes=Inf, sampleIndex=1:length(y),
                   splitrule="variance", avgfunc=avgMean){
  # Create the list of nodeSize and sampleIndex
  aggregateNodeSize <- list(
    "averagingNodeSize"=nodesize,
    "splittingNodeSize"=nodesize
  )
  aggregateSampleIndex <- list(
    "averagingSampleIndex"=sampleIndex,
    "splittingSampleIndex"=sampleIndex
  )
  # Create a original random forest tree.
  tree <- new("RFTree",
              x=x,
              y=y,
              mtry=mtry,
              nodesize=aggregateNodeSize,
              maxnodes=maxnodes,
              sampleIndex=aggregateSampleIndex,
              splitrule=splitrule,
              avgfunc=avgfunc,
              root=NULL,
              totalNodes=0,
              totalTerminalNodes=0)

  # Grow the tree.
  root <- recursivePartition(
    tree, x=x, y=y, mtry=mtry, nodesize=aggregateNodeSize, maxnodes=maxnodes,
    sampleInde=aggregateSampleIndex, splitrule=splitrule, avgfunc=avgfunc
  )
  tree@root <- root
  return(tree)
}


######################
### Generic Method ###
######################
#' selectBestFeature-RFTree
#' @name selectBestFeature-RFTree
#' @rdname selectBestFeature-RFTree
#' @description Find the best `splitfeature`, `splitValue` pair where
#' `splitfeature` is one of the features specified by `featureList`. The
#' `splitFeature` and its corresponding `splitValue` minimizes the specified
#' `splitrule`. The implementation is slightly different from the original
#' implementation as the tree contains both averaging and splitting dataset.
#' To check the minimum split, the method checks for both datasets according to
#' `sampleIndex` and `nodesize`.
#' @param x Data frame or a matrix of all predictors.
#' @param y Response vector.
#' @param featureList A list of candidate variables at the current split.
#' @param sampleIndex A list of index of dataset used in this node and its
#' children. `sampleIndex` contains two keys `averagingSampleIndex`
#' and `splittingSampleIndex`. `averagingSampleIndex` is used to generate
#' aggregated prediction for the node. `splittingSampleIndex` is used for
#' `honestRF` which stores the splitting data when creating the tree. In
#' default, `splittingSampleIndex` is the same as `averagingSampleIndex`.
#' @param nodesize Minimum size of terminal nodes. Setting this number larger
#' causes smaller trees to be grown (and thus take less time). This parameter is
#' actually a list containing the values for both `splittingNodeSize` and
#' `averagingNodeSize`.
#' @param maxnodes Maximum number of terminal nodes trees in the forest can
#' have. If not given, trees are grown to the maximum possible (subject to
#' limits by nodesize).
#' @param splitrule A splitting rule to determine the best split point among the
#' features. There are two possible split rules in the package, `variance` and
#' `maxstats`. The default is `variance` to minimize the overall MSE.
#' @return A list of two outputs: "splitFeature" is the best feature to split
#' in order to minimize the split loss, "splitValue" is its corresponding split
#' value.
selectBestFeature <- function(x, y, featureList,
                              sampleIndex=list(
                                "averagingSampleIndex"=1:length(y),
                                "splittingSampleIndex"=1:length(y)
                              ),
                              nodesize=list(
                                "splittingNodeSize"=5,
                                "averagingNodeSize"=5
                              ),
                              maxnodes=Inf,
                              splitrule="variance"){
    mtry <- length(featureList)

    # Initialize the minimum loss for each feature
    bestSplitLossAll <- rep(-Inf, mtry)
    bestSplitValueAll <- rep(NA, mtry)
    bestSplitFeatureAll <- rep(NA, mtry)
    bestSplitCountAll <- rep(0, mtry)

    # Iterate each selected features
    for (i in 1:mtry) {
      currentFeature <- featureList[i]

      # Test if the all the values for the feature are the same, proceed
      if (length(unique(x[sampleIndex$splittingSampleIndex, currentFeature]))
          == 1) next()

      oldFeatureValue <- min(x[sampleIndex$splittingSampleIndex,
                               currentFeature])
      allFeatureValueSorted <- sort(
        unique(x[sampleIndex$splittingSampleIndex, currentFeature])
        )[-1]

      #     # Potential SpeedUP:
      #     iorder <- order(feat.J[ , thisfeat])
      #     feati.J.sort <- feat.J[iorder, thisfeat]
      #     tr.J.sort <- tr.J[iorder]
      #     yobs.J.sort <- yobs.J[iorder]
      #

      # Go through all the values in the selected feature
      for (featureValue in allFeatureValueSorted) {

        # Make partitions on the current feature and value in both splitting and
        # averaging dataset.
        leftPartitionAveraging <-
          x[sampleIndex$averagingSampleIndex, currentFeature] < featureValue
        leftPartitionSplitting <-
          x[sampleIndex$splittingSampleIndex, currentFeature] < featureValue

        leftPartitionCountSplitting <- sum(leftPartitionSplitting)
        rightPartitionCountSplitting <- sum(!leftPartitionSplitting)
        leftPartitionCountAveraging <- sum(leftPartitionAveraging)
        rightPartitionCountAveraging <- sum(!leftPartitionAveraging)

        # Check leaf size at least nodesize
        if (
          min(leftPartitionCountSplitting,
              rightPartitionCountSplitting) < nodesize$splittingNodeSize |
          min(leftPartitionCountAveraging,
              rightPartitionCountAveraging) < nodesize$averagingNodeSize){
          # Update the oldFeature value before proceeding
          oldFeatureValue <- featureValue
          next()
        }

        # Calculate sample mean in both splitting partitions
        leftPartitionMean <-
          mean(y[sampleIndex$splittingSampleIndex][leftPartitionSplitting])
        rightPartitionMean <-
          mean(y[sampleIndex$splittingSampleIndex][!leftPartitionSplitting])

        # Calculate the variance of the splitting
        muBarSquareSum <- leftPartitionCountSplitting * leftPartitionMean ^ 2 +
          rightPartitionCountSplitting * rightPartitionMean ^ 2

        # Update the value if a higher value has been seen
        if (muBarSquareSum > bestSplitLossAll[i]) {
          bestSplitLossAll[i] <- muBarSquareSum
          bestSplitFeatureAll[i] <- currentFeature
          bestSplitValueAll[i] <- runif(1, oldFeatureValue, featureValue)
          bestSplitCountAll[i] <- 1
        } else {
          # If we are as good as the best split
          if (muBarSquareSum == bestSplitLossAll[i]) {
            bestSplitCountAll[i] <- bestSplitCountAll[i] + 1
            # Only update with probability 1/nseen
            if (runif(1, 0, bestSplitCountAll[i]) <= 1){
              bestSplitLossAll[i] <- muBarSquareSum
              bestSplitFeatureAll[i] <- currentFeature
              bestSplitValueAll[i] <- runif(1, oldFeatureValue, featureValue)
            }
          }
        }
        # Update the old feature value
        oldFeatureValue <- featureValue
      }
    }

    # Get the best split values among all features
    bestSplitLoss <- max(bestSplitLossAll)
    bestFeatureIndex <- which(bestSplitLossAll == bestSplitLoss)


    if (sum(bestSplitCountAll) > 0) {
      # If there are multiple best features, sample one according to their
      # frequency of occurence
      elements_to_sample <- rep(bestFeatureIndex,
                                bestSplitCountAll[bestFeatureIndex])
      bestFeatureIndex <- ifelse(length(elements_to_sample) == 1,
                                 elements_to_sample,
                                 sample(elements_to_sample, 1))
    }else{
      # If none of the features are possible, return NA
      return(list("bestSplitFeature"=NA, "bestSplitValue"=NA))
    }

    # Return the best splitFeature and splitValue
    return(list("bestSplitFeature"=bestSplitFeatureAll[bestFeatureIndex],
                "bestSplitValue"=bestSplitValueAll[bestFeatureIndex]))
}

#' recursivePartition-RFTree
#' @name recursivePartition-RFTree
#' @rdname recursivePartition-RFTree
#' @description Grow the decision tree by recursively finding the best split
#' feature and value.
#' @param x A data frame or a matrix of all predictors.
#' @param y A response vector.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#' The default value is set to be one third of total feature amount.
#' @param sampleIndex A list of index of dataset used in this node and its
#' children. `sampleIndex` contains two keys `averagingSampleIndex`
#' and `splittingSampleIndex`. `averagingSampleIndex` is used to generate
#' aggregated prediction for the node. `splittingSampleIndex` is used for
#' `honestRF` which stores the splitting data when creating the tree. In
#' default, `splittingSampleIndex` is the same as `averagingSampleIndex`.
#' @param nodesize Minimum size of terminal nodes. Setting this number larger
#' causes smaller trees to be grown (and thus take less time). This parameter is
#' actually a list containing the values for both `splittingNodeSize` and
#' `averagingNodeSize`.
#' @param maxnodes Maximum number of terminal nodes trees in the forest can have.
#' If not given, trees are grown to the maximum possible (subject to limits by
#' nodesize).
#' @param splitrule A splitting rule to determine the best split point among the
#' features. There are two possible split rules in the package, `variance` and
#' `maxstats`. The default is `variance` to minimize the overall MSE.
#' @param avgfunc An averaging function to average all the input data. The input
#' of this function should be a dataframe of predictors `x` and a vector of
#' outcome `y`. The output is a scalar. The default is to take the mean of all
#' the `y`s.
#' @return root node.
#' @exportMethod recursivePartition
setGeneric(
  name="recursivePartition",
  def=function(theObject, x, y, mtry, sampleIndex, nodesize, maxnodes,
               splitrule, avgfunc){
    standardGeneric("recursivePartition")
  }
)

#' @rdname recursivePartition-RFTree
#' @aliases recursivePartition, RFTree-method
setMethod(
  f="recursivePartition",
  signature="RFTree",
  definition=function(theObject, x, y,
                      mtry=max(floor(ncol(x)/3), 1),
                      sampleIndex=list(
                        "averagingSampleIndex"=1:length(y),
                        "splittingSampleIndex"=1:length(y)
                      ),
                      nodesize=list(
                        "splittingNodeSize"=5,
                        "averagingNodeSize"=5
                      ),
                      maxnodes=Inf,
                      splitrule="variance",
                      avgfunc=avgMean
                      ){

    # Sample features for the split
    selectedFeatureIndex <- sample(1:ncol(x), mtry)

    # Load bestSplitFeature, bestSplitValue to the current environment
    list2env(
      selectBestFeature(
        x=x,
        y=y,
        featureList=selectedFeatureIndex,
        sampleIndex=sampleIndex,
        nodesize=nodesize,
        maxnodes=maxnodes,
        splitrule=splitrule),
      environment())

    # Create a leaf node if the current bestSplitValue is NA
    if (is.na(bestSplitValue)) {
      theObject@totalTerminalNodes <- theObject@totalTerminalNodes + 1
      theObject@totalNodes <- theObject@totalNodes + 1
      return(
        RFNode(x=x,
               y=y,
               splitFeature=NULL,
               splitValue=NULL,
               sampleIndex=sampleIndex,
               avgfunc=avgfunc))
    } else {
      # Create a tree node
      # Update sample index for both left and right partitions
      leftPartitionSplitting <-
        x[sampleIndex$splittingSampleIndex,
          bestSplitFeature] < bestSplitValue
      leftPartitionAveraging <-
        x[sampleIndex$averagingSampleIndex,
          bestSplitFeature] < bestSplitValue
      sampleIndex_left <- list(
        "splittingSampleIndex"=
          sampleIndex$splittingSampleIndex[leftPartitionSplitting],
        "averagingSampleIndex"=
          sampleIndex$averagingSampleIndex[leftPartitionAveraging]
      )
      sampleIndex_right <- list(
        "splittingSampleIndex"=
          sampleIndex$splittingSampleIndex[!leftPartitionSplitting],
        "averagingSampleIndex"=
          sampleIndex$averagingSampleIndex[!leftPartitionAveraging]
      )

      # Recursively grow the tree
      leftChild <- recursivePartition(
        theObject, x, y, mtry=mtry, nodesize=nodesize, maxnodes=maxnodes,
        sampleIndex=sampleIndex_left,
        splitrule=splitrule, avgfunc=avgfunc)

      rightChild <- recursivePartition(
        theObject, x, y, mtry=mtry, nodesize=nodesize, maxnodes=maxnodes,
        sampleIndex=sampleIndex_right,
        splitrule=splitrule, avgfunc=avgfunc)

      # Create the leaf node the connects to both children
      theObject@totalNodes <- theObject@totalNodes + 1
      node <- RFNode(x=x, y=y, splitFeature=bestSplitFeature,
                     splitValue=bestSplitValue, sampleIndex=sampleIndex,
                     avgfunc=avgfunc)
      node@isLeaf <- FALSE
      node@leftChild <- leftChild
      node@rightChild <- rightChild
      return(node)
    }
  }
)

#' predict-RFTree
#' @name predict-RFTree
#' @rdname predict-RFTree
#' @description Predict the regression value for `x` by calling `predict(x)`
#' from the root.
#' @param theObject RFTree object
#' @param feature.new A data frame or a matrix of all testing predictors
#' @exportMethod predict
if (!isGeneric("predict")) {
  setGeneric(
    name="predict",
    def=function(theObject, feature.new){
      standardGeneric("predict")
    }
  )
}

#' @rdname predict-RFTree
#' @aliases predict, RFTree-method
setMethod(
  f="predict",
  signature="RFTree",
  definition=function(theObject, feature.new){
    return(predict(theObject@root, feature.new))
  }
)

#' showTree-RFTree
#' @name showTree-RFTree
#' @rdname showTree-RFTree
#' @description Print the entire tree starting from the root node
#' @param theObject RFTree object
#' @exportMethod showTree
setGeneric(
  name="showTree",
  def=function(theObject){
    standardGeneric("showTree")
  }
)

#' @rdname showTree-RFTree
#' @aliases showTree, RFTree-method
setMethod(
  f="showTree",
  signature="RFTree",
  definition = function(theObject){
    printnode(theObject@root)
  }
)
