################################################
### Honest Random Forest Implementation in R ###
################################################

# @title validSplitTest
# @description Determine if a split is valid. If a split is valid, it must both
# have at least minSizeSplit observation in both two splitted subsets, and also 
# in both splitting and averaging datasets.
# @param splitFeature The name of the feature that we are trying to split
# @param splitValue The split value of the selected feature 
# @param xSplit Observations in the splitting dataset
# @param minSizeSplit Minimum number of observations required in each of the
# splitted splitting dataframe
# @param xAverage Observations in the averaging dataset 
# @param minSizeAverage Minimum number of observations required in each of the
# splitted averaging dataframe
# @return TRUE/FALSE to indicate if the current feature and split value satisfy 
# valid split requirement
validSplitTest <- function(splitFeature, splitValue, xSplit, minSizeSplit=5, 
                           xAverage=NULL, 
                           minSizeAverage=if (!is.null(xAverage)) 5 else NULL) {
  # Get all the observations that have less split value
  xSplitLeft <- xSplit[xSplit[splitFeature] < splitValue, ]
  # Test if the left part has less than required observations or the right part
  # has less than required observations
  if ((nrow(xSplitLeft) < minSizeSplit) ||
      (nrow(xSplitLeft) > (nrow(xSplit) - minSizeSplit))) {
    # Bad split for splitting dataset
    return(FALSE)
  } 
  
  # If average dataset is provided, test if the corresponding left part and 
  # right part satisfy the requirement
  if (!is.null(xAverage)) {
    xAverageLeft = xAverage[xAverage[splitFeature] < splitValue, ]
    if ((nrow(xAverageLeft) < minSizeAverage) ||
        (nrow(xAverageLeft) > (nrow(xAverage) - minSizeAverage))) {
      # Bad split for averaging dataset
      return(FALSE)
    }
  }
  
  # Pass all the test and return TRUE
  return(TRUE)
}

# @title splitHelper
# @description Split both the feature dataframe and outcome dataframe according
# to a selected feature and split value
# @param splitFeature The name of the feature that we are trying to split
# @param splitValue The split value of the selected feature 
# @param x Observation dataframe to split 
# @param y Outcome dataframe to split 
# @return A list of splitted dataframes: xLeft, xRight, yLeft, yRight are the
# names corresponding to the the left and right parts of feature and outcome
# dataframe
splitHelper <- function(splitFeature, splitValue, x, y) {
  # If x and y are null, it will return null for all partitions
  if (is.null(x)) {
    return(list(
      xLeft=NULL,
      xRight=NULL,
      yLeft=NULL,
      yRight=NULL
      ))
  } else {
    return(list(
      xLeft=x[x[splitFeature] < splitValue, ],
      xRight=x[x[splitFeature] >= splitValue, ],
      yLeft=y[x[splitFeature] < splitValue],
      yRight=y[x[splitFeature] >= splitValue]
    ))
  }
}

# @title splitCriteriaDeviance
# @description A default function to measure the loss of the splitting
# @param yLeft The splitted outcomes whose corresponding features are less than
# the splitted value
# @param yRight The splitted outcomes whose corresponding features are bigger
# than the splitted value
# @return The deviance of the splitting
splitCriteriaDeviance <- function(yLeft, yRight) {
  return(
    sum((yLeft - mean(yLeft))^2) +
      sum((yRight - mean(yRight))^2)
  )
}

# @title selectFeatureBestSplit
# @description Find the best split value for the selected feature. The split 
# point minimizes the specified splitting criteria. 
# @param splitFeature The name of the feature that we are trying to split
# @param xSplit Observations in the splitting dataset
# @param ySplit Outcomes in the splitting dataset
# @param minSizeSplit Minimum number of observations required in each of the
# splitted splitting dataframe
# @param xAverage Observations in the averaging dataset 
# @param minSizeAverage Minimum number of observations required in each of the
# splitted averaging dataframe
# @param splitCriteria A function that measure the splitting loss
# @return A list of two outputs: "SplitValue" is the best split value for the 
# selected feature in order to minimize the split loss, "SplitLoss" is the 
# corresponding minimal loss value
selectFeatureBestSplit <- function(splitFeature, xSplit, ySplit, minSizeSplit=5,
                                   xAverage=NULL, 
                                   minSizeAverage=
                                     if (!is.null(xAverage)) 5 else NULL,
                                   splitCriteria=splitCriteriaDeviance) {
  # Initialize the bests as inf
  bestSplitError <- Inf
  bestSplitValue <- NA
  
  # Iterate through all possible sample values
  for (splitSampleValue in xSplit[, splitFeature]) {
    # First test if the splitSampleValue is a valid splitting point
    if (validSplitTest(splitFeature, splitSampleValue, xSplit, minSizeSplit, 
                       xAverage, minSizeAverage)) {
      # Split according to the current splitSampleValue
      splitSampleData <- splitHelper(splitFeature, splitSampleValue, xSplit, 
                                     ySplit)
      # Calculate the corresponding error
      splitSampleError <- splitCriteria(splitSampleData$yLeft, 
                                        splitSampleData$yRight)
      # Update the best error and split value if it is better
      if (splitSampleError < bestSplitError) {
        bestSplitError <- splitSampleError
        bestSplitValue <- splitSampleValue
      }
    }
  }
  
  return(list(
    splitError=bestSplitError,
    splitValue=bestSplitValue
  ))
}

# @title selectBestSplit
# @description Find the best split value for all features. The split feature and
# its corresponding split value minimizes the specified splitting criteria. 
# @param xSplit Observations in the splitting dataset
# @param ySplit Outcomes in the splitting dataset
# @param minSizeSplit Minimum number of observations required in each of the
# splitted splitting dataframe
# @param xAverage Observations in the averaging dataset 
# @param minSizeAverage Minimum number of observations required in each of the
# splitted averaging dataframe
# @param nFeatures Number of variables randomly sampled as candidates at each 
# split.
# @param splitCriteria A function that measure the splitting loss
# @return A list of three outputs: "splitFeature" is the best feature to split 
# in order to minimize the split loss, "splitValue" is its corresponding split 
# value, and "splitLoss" is the corresponding minimal loss value
selectBestSplit <- function(xSplit, ySplit,
                            minSizeSplit=5, 
                            xAverage=NULL, 
                            minSizeAverage=if (!is.null(xAverage)) 5 else NULL,
                            nFeatures=max(floor(ncol(xSplit)/3), 1),
                            splitCriteria=splitCriteriaDeviance){
  # Initialize the bests as inf
  bestSplitError <- Inf
  bestSplitFeature <- NA
  bestSplitValue <- NA
  # Sample features in the current split
  featureNames = if (is.null(colnames(xSplit))) 1:ncol(xSplit) 
                 else colnames(xSplit) 
  sampledFeatures = sample(featureNames, nFeatures, replace=FALSE)
  # Iterate through all possible sample features
  for (splitSampleFeature in sampledFeatures) {
    sampleFeatureBestSplit <- selectFeatureBestSplit(splitSampleFeature, xSplit, 
                                                     ySplit, minSizeSplit,
                                                     xAverage, minSizeAverage,
                                                     splitCriteria)
    # Test if the feature splitting is valid
    if (!is.na(sampleFeatureBestSplit$splitValue)) {
      # Update the best error, split feature and split value if it is better
      if (sampleFeatureBestSplit$splitError < bestSplitError) {
        bestSplitError <- sampleFeatureBestSplit$splitError
        bestSplitValue <- sampleFeatureBestSplit$splitValue
        bestSplitFeature <- splitSampleFeature
      }
    }
  }
  
  return(list(
    splitFeature=bestSplitFeature,
    splitError=bestSplitError,
    splitValue=bestSplitValue
  ))
}

# @title averageMetricsMean
# @description A default function to average the observations in each tree leaf
# by taking its mean
# @param x Observation dataframe in the current tree leaf
# @param y Outcome dataframe in the current tree leaf
# @return The averaging metrics of the splitting
averageMetricsMean <- function(x, y) {
  return(mean(y))
}

# @title recursivePartition
# @description Use recursive partitioning to grow the tree, either creating 
# a good child split for a best feature choice or making averaging for the 
# observations in the leaf node.
# @param currentDepth The current depth into growing the tree
# @param xSplit Observations in the splitting dataset
# @param ySplit Outcomes in the splitting dataset
# @param minSizeSplit Minimum number of observations required in each of the
# splitted splitting dataframe
# @param xAverage Observations in the averaging dataset 
# @param yAverage Outcomes in the averaging dataset
# @param minSizeAverage Minimum number of observations required in each of the
# splitted averaging dataframe
# @param nFeatures Number of variables randomly sampled as candidates at each 
# split.
# @param maxDepth Maximum number of levels that a tree can reach to. If it is 
# NULL, the tree can grow infinitely but will terminate by minSizeSplit
# @param splitCriteria A function that measure the splitting loss
# @param averageMetrics A function that averages the observations in the 
# terminal nodes
# @return A root node of the tree that keeps track of the children information 
# for the partition. 
recursivePartition <- function(currentDepth, xSplit, ySplit, 
                               minSizeSplit=5, xAverage=NULL, yAverage=NULL, 
                               minSizeAverage=
                                 if (!is.null(yAverage)) 5 else NULL,
                               nFeatures=max(floor(ncol(xSplit)/3), 1),
                               maxDepth=NULL, 
                               splitCriteria=splitCriteriaDeviance, 
                               averageMetrics=averageMetricsMean) {

  # Select the best features and split value for the current dataset
  bestSplit <- selectBestSplit(xSplit, ySplit, minSizeSplit, 
                               xAverage, minSizeAverage, nFeatures, 
                               splitCriteria)
  
  # Base case: If we reach the maximum depth, simply aggregate the current 
  # input. In addition, if the best split is not valid, also return the 
  # aggregation of the current input. 
  if ((!is.null(maxDepth) && currentDepth >= maxDepth) || 
      (is.na(bestSplit$splitValue))) {
    if (!is.null(yAverage)) {
      treeRoot <- averageMetrics(xAverage, yAverage)
    } else {
      treeRoot <- averageMetrics(xSplit, ySplit)
    }
    return(treeRoot)
  }
  
  # Split splitting data according to the selected feature and split point
  splitDataNew <- splitHelper(
    bestSplit$splitFeature, 
    bestSplit$splitValue, 
    xSplit, ySplit
  )
  averageDataReal <- splitHelper(
    bestSplit$splitFeature, 
    bestSplit$splitValue, 
    xAverage, yAverage
  )

  # If averaging dataset is there, split averaging dataset as well. If averaging
  # dataset is not there, we pretent to average over splitting dataset but its 
  # real values are all null
  if (!is.null(yAverage)) {
    averageDataNew <- averageDataReal
  } else {
    averageDataNew <- splitDataNew
  }
  
  # Update treeRoot for the current split information
  treeRoot <- list(
    splitFeature=bestSplit$splitFeature,
    splitValue=bestSplit$splitValue,
    splitError=bestSplit$splitError,
    leftChild=recursivePartition(
      currentDepth+1, splitDataNew$xLeft, splitDataNew$yLeft,
      minSizeSplit, xAverage=averageDataReal$xLeft,
      yAverage=averageDataReal$yLeft, minSizeAverage,
      nFeatures, maxDepth, splitCriteria, averageMetrics
    ),
    rightChild=recursivePartition(
      currentDepth+1, splitDataNew$xRight, splitDataNew$yRight,
      minSizeSplit, xAverage=averageDataReal$xRight,
      yAverage=averageDataReal$yRight, minSizeAverage,
      nFeatures, maxDepth, splitCriteria, averageMetrics
    )
  )
  return(treeRoot)
}

# @title buildDecisionTree
# @description Given the observations and outcomes, build a decision tree using
# selected features
# @param xSplit Observations in the splitting dataset
# @param ySplit Outcomes in the splitting dataset
# @param minSizeSplit Minimum number of observations required in each of the
# splitted splitting dataframe
# @param xAverage Observations in the averaging dataset 
# @param yAverage Outcomes in the averaging dataset
# @param minSizeAverage Minimum number of observations required in each of the
# splitted averaging dataframe
# @param nFeatures Number of variables randomly sampled as candidates at each 
# split.
# @param maxDepth Maximum number of levels that a tree can reach to. If it is 
# NULL, the tree can grow infinitely but will terminate by minSizeSplit
# @param splitCriteria A function that measure the splitting loss
# @param averageMetrics A function that averages the observations in the 
# terminal nodes
# @return A root node of the tree that keeps track of the children information 
# for the partition. When this function is run at the beginning, it should be 
# an empty list and will be appended as the tree recursively grows
buildDecisionTree <- function(xSplit, ySplit, minSizeSplit=5, 
                              xAverage=NULL, yAverage=NULL, 
                              minSizeAverage=
                                if (!is.null(yAverage)) 5 else NULL,
                              nFeatures=max(floor(ncol(xSplit)/3), 1),
                              maxDepth=NULL, 
                              splitCriteria=splitCriteriaDeviance, 
                              averageMetrics=averageMetricsMean) {
  treeRoot <- recursivePartition(
    0, xSplit, ySplit, minSizeSplit, xAverage, yAverage,
    minSizeAverage, nFeatures, maxDepth, splitCriteria, averageMetrics
  )
  return(treeRoot)
}

# @title predictSingleObservation
# @description Once the decision tree is built, given the treeRoot to find the 
# predicted value for one given observation
# @param treeRoot A root node of the tree that keeps track of the children
# information for the partition
# @param x One single data entry of an observation
# @return The predicted value for the input observation
predictSingleObservation <- function(treeRoot, x){
  if (x[treeRoot$splitFeature] < treeRoot$splitValue) {
    if (is(treeRoot$leftChild, "list")) {
      # Found a child
      return(predictSingleObservation(treeRoot$leftChild, x))
    } else {
      # Not a child, return the aggregated prediction
      return(treeRoot$leftChild)
    }
  } else {
    if (is(treeRoot$rightChild, "list")) {
      # Found a child
      return(predictSingleObservation(treeRoot$rightChild, x))
    } else {
      # Not a child, return the aggregated prediction
      return(treeRoot$rightChild)
    }
  }
}

# @title predictDecisionTree
# @description Once the decision tree is built, given the treeRoot to find the 
# predicted value for a given dataframe of observations
# @param treeRoot A root node of the tree that keeps track of the children
# information for the partition
# @param x A dataframe of observations
# @return A dataframe of predicted outcomes for the input observations
predictDecisionTree <- function(treeRoot, x){
  return(apply(x, 1, predictSingleObservation, treeRoot=treeRoot))
}


# @title predictHonestRF
# @description Given a list of treeRoots, aka decision trees, we use bagging to
# combine their predictions together by taking the average of those predictions
# @param treeRoots A list of root node of decision trees
# @param x A dataframe of observations
# @return A dataframe of predicted outcomes for the input observations
predictHonestRF <- function(treeRoots, x){
  y_preds <- data.frame(row.names=1:nrow(x))
  # Append predictions from each decision tree together
  for (treeRoot in treeRoots) {
    y_preds <- cbind(y_preds, predictDecisionTree(treeRoot, x))
  }
  # Return the mean of all trees
  return(apply(y_preds, 1, mean))
}

# @title buildHonestRF
# @description Given the observations and outcomes, build a honest random forest
# @param xSplit Observations in the splitting dataset
# @param ySplit Outcomes in the splitting dataset
# @param minSizeSplit Minimum number of observations required in each of the
# splitted splitting dataframe (default = 5)
# @param xAverage Observations in the averaging dataset 
# @param yAverage Outcomes in the averaging dataset
# @param minSizeAverage Minimum number of observations required in each of the
# splitted averaging dataframe (default = 5 if average dataset is not null)
# @param nFeatures Number of variables randomly sampled as candidates at each 
# split. (Default = p/3 for regression task)
# @param maxDepth Maximum number of levels that a tree can reach to. If it is 
# NULL, the tree can grow infinitely but will terminate by minSizeSplit
# @param nTrees Number of trees to grow (Default = 500)
# @param replace Indictor of if sampling is with replacement
# @param sampSizeSplit Size of sample to draw for splitting dataset
# @param sampSizeAverage Size of sample to draw for averaging dataset. 
# @param splitCriteria A function that measure the splitting loss
# @param averageMetrics A function that averages the observations in the 
# terminal nodes
# @return A list of root node of decision trees
buildHonestRF <- function(xSplit, ySplit, minSizeSplit=5, 
                          xAverage=NULL, yAverage=NULL, 
                          minSizeAverage=
                            if (!is.null(yAverage)) 5 else NULL,
                          nFeatures=max(floor(ncol(xSplit)/3), 1),
                          maxDepth=NULL, 
                          nTrees=500,
                          replace=TRUE,
                          sampSizeSplit=
                            if (replace) nrow(xSplit)
                              else ceiling(.632*nrow(xSplit)),
                          sampSizeAverage=
                            if (!is.null(xAverage) && replace) nrow(xAverage) 
                              else if (!is.null(xAverage)) 
                                ceiling(.632*nrow(xAverage)) 
                              else NULL,
                          splitCriteria=splitCriteriaDeviance, 
                          averageMetrics=averageMetricsMean) {
  # Initialize a vector of tree nodes
  treeRoots <- vector("list", length=nTrees)
  
  # Iterate through all the trees
  for (i in 1:nTrees) {
    # sample splitting dataset
    sampledSplitIndex <- sample.int(nrow(xSplit), sampSizeSplit, 
                                    replace=replace)
    xSplitSampled <- xSplit[sampledSplitIndex, ]
    ySplitSampled <- ySplit[sampledSplitIndex]
    
    # if averaging dataset is present, sample as well. Otherwise NULL
    if (!is.null(xAverage)) {
      sampledAverageIndex <- sample.int(nrow(xAverage), sampSizeAverage, 
                                        replace=replace)
      xAverageSampled <- xAverage[sampledAverageIndex, ]
      yAverageSampled <- yAverage[sampledAverageIndex]
    } else {
      xAverageSampled <- NULL
      yAverageSampled <- NULL
    }
    
    # Use sampled dataset to build a decision tree
    treeRoots[[i]] <- buildDecisionTree(xSplitSampled, ySplitSampled, 
                                        minSizeSplit, xAverageSampled, 
                                        yAverageSampled, minSizeAverage, 
                                        nFeatures, maxDepth, splitCriteria, 
                                        averageMetrics)
  }
  return(treeRoots)
}