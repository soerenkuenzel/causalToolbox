#############################################
### Honest Random Forest Tree Constructor ###
#############################################
#' @title honstRFTree Constructor
#' @name honestRFTree-class
#' @rdname honestRFTree-class
#' @description `honestRFTree` inherits `RFTree`, which serves as a modified
#' version of `RFTree`. The major change is that when `honestRFTree` determines
#' a valid split point, it looks at whether or not it is valid for both
#' averaging and splitting dataset.
#' @exportClass honestRFTree
setClass(
  Class="honestRFTree",
  contains="RFTree"
)

#' @title honstRFTree Constructor
#' @name honestRFTree
#' @rdname honestRFTree-class
#' @description Create a honestRFTree by making specifc observatios as splitting
#' and averaging dataset.
#' @param x A data frame of all training predictors.
#' @param y A vector of all training responses.
#' @param mtry The number of variables randomly selected at each split point.
#' The default value is set to be one third of total number of features of the
#' training data.
#' @param nodesize The minimum observations contained in terminal nodes. The
#' default value is 5.
#' @param sampleIndex A list of the index of observations that are used as
#' averaging dataset. The index are based on the original dataset `x` and `y`
#' from forest. Essentially, `x[sampleIndex]` generates the whole splitting
#' dataset.
#' @param splitrule A string to specify how to find the best split among all
#' candidate feature values. The current version only supports `variance` which
#' minimizes the overall MSE after splitting. The default value is `variance`.
#' @export honestRFTree
setGeneric(
  name="honestRFTree",
  def=function(
    x,
    y,
    mtry,
    nodesize,
    sampleIndex,
    splitrule
    ){
    standardGeneric("honestRFTree")
  }
)

#' @title honstRFTree Constructor
#' @rdname honestRFTree-honestRFTree
#' @aliases honestRFTree, honestRFTree-method
#' @return a `honestRFTree` object
honestRFTree <- function(
  x,
  y,
  mtry=max(floor(ncol(x)/3), 1),
  nodesize=list(
   "averagingNodeSize"=5,
   "splittingNodeSize"=5
  ),
  sampleIndex=list(
   "averagingSampleIndex"=1:length(y),
   "splittingSampleIndex"=1:length(y)
  ),
  splitrule="variance"
  ){

  if(nodesize$averagingNodeSize > length(sampleIndex$averagingSampleIndex)){
    warning("For at least one tree, the averagingNodeSize is chosen to be bigger
            than the entier data set. It is not set to be equal to the
            length of the entier data set.")
    nodesize$averagingNodeSize <- length(sampleIndex$averagingSampleIndex)
  }
  if(nodesize$splittingNodeSize > length(sampleIndex$splittingSampleIndex)){
    warning("For at least one tree, the splittingNodeSize is chosen to be bigger
            than the entier data set. It is not set to be equal to the
            length of the entier data set.")
    nodesize$splittingNodeSize <- length(sampleIndex$splittingSampleIndex)
  }
  if(length(sampleIndex$splittingSampleIndex) == 0 |
     length(sampleIndex$averagingSampleIndex) == 0){
    stop("Either splitting or averaging set is empty.")
  }


  tree <- new(
    "honestRFTree",
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

