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
honestRFTree <- function(x, y,
                         mtry=max(floor(ncol(x)/3), 1),
                         nodesize=list(
                           "averagingNodeSize"=5,
                           "splittingNodeSize"=5
                         ),
                         maxnodes=Inf,
                         sampleIndex=list(
                           "averagingSampleIndex"=1:length(y),
                           "splittingSampleIndex"=1:length(y)
                         ),
                         splitrule="variance", avgfunc=avgMean){

  tree <- new("honestRFTree",
              x=x,
              y=y,
              mtry=mtry,
              nodesize=nodesize,
              maxnodes=maxnodes,
              sampleIndex=sampleIndex,
              splitrule=splitrule,
              avgfunc=avgfunc,
              root=NULL,
              totalNodes=0,
              totalTerminalNodes=0)

  # Grow the tree.
  root <- recursivePartition(
    tree, x=x, y=y, mtry=mtry, nodesize=nodesize, maxnodes=maxnodes,
    sampleInde=sampleIndex, splitrule=splitrule, avgfunc=avgfunc
  )

  tree@root <- root
  return(tree)
}

