#############################################
### Honest Random Forest Tree Constructor ###
#############################################

#' @name honestRFTree-class
#' @rdname honestRFTree-class
#' @exportClass honestRFTree
#' @description `honestRFTree` inherits `RFTree`, which serves as a modified
#' version of `RFTree`. The major change is that when `honestRFTree` determines
#' a valid split point, it looks at whether or not it is valid for both
#' averaging and splitting dataset.
setClass(
  Class="honestRFTree",
  contains="RFTree"
)

#' honestRFTree Constructor
#' @name honestRFTree
#' @rdname honestRFTree-class
#' @param x A data frame or a matrix of all training predictors.
#' @param y A vector of all training responses.
#' @param mtry Number of variables randomly sampled as candidates at each split.
#' The default value is set to be one third of total feature amount.
#' @param nodesize Minimum size of terminal nodes. Setting this number larger
#' causes smaller trees to be grown (and thus take less time). For the
#' `honestRFTree`, the minimum size of nodes need to be specified for both
#' averaging dataset and splitting dataset. The default value for both is 5.
#' @param sampleIndex A list of the index of observations that are used as
#' averaging dataset. The index are based on the original dataset `x` and `y`
#' from forest. Essentially, `x[sampleIndex]` generates the whole splitting
#' dataset. For the `honestRFTree`, the sampleIndex need to be specified for
#' both averaging dataset and splitting dataset. The default value for both is
#' all the dataset of input dataset.
#' @param splitrule A splitting rule to determine the best split point among the
#' features. There are two possible split rules in the package, `variance` and
#' `maxstats`. The default is `variance` to minimize the overall MSE.
#' @return a `honestRFTree` object
honestRFTree <- function(x, y,
                         mtry=max(floor(ncol(x)/3), 1),
                         nodesize=list(
                           "averagingNodeSize"=5,
                           "splittingNodeSize"=5
                         ),
                         sampleIndex=list(
                           "averagingSampleIndex"=1:length(y),
                           "splittingSampleIndex"=1:length(y)
                         ),
                         splitrule="variance"){

  tree <- new("honestRFTree",
              sampleIndex=sampleIndex,
              root=list()
              )

  # Grow the tree.
  root <- recursivePartition(
    x=x,
    y=y,
    mtry=mtry,
    nodesize=nodesize,
    sampleIndex=sampleIndex,
    splitrule=splitrule
  )

  tree@root <- list("node"=root)
  return(tree)
}

