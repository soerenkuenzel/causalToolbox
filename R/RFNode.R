#####################
### Static Method ###
#####################
#' @title avgMean
#' @description A default function to average the observations in each tree leaf
#' by taking the mean of the outcomes.
#' @param x A dataframe of observations in the current tree leaf
#' @param y A vector of outcomes in the current tree leaf
#' @return The averaging metrics of the data
avgMean <- function(x, y) {
  return(mean(y))
}

######################################
### Random Forest Node Constructor ###
######################################
#' @name RFNode-class
#' @rdname RFNode-class
#' @exportClass RFNode
#' @description `RFNode` is the basic element inside a `RFTree`. For each node,
#' it contains the corresponding data that are assigned to the node. The
#' `RFNode` can be either a leaf and a tree node (non-leaf). If it is a leaf
#' node, `avgfunc` can be called to aggregate all the observations in the node
#' and save a prediction for the node. If it is a tree node, it will contains
#' `leftChild` and `rightChild` which will be another two `RFNode`s. The parent
#' node can be used to track the `splitFeature` and `splitValue`. Please note
#' that in the `RFNode`, `sampleIndex` essentially tells which data that reside
#' in the node. Every node contains two `sampleIndex`: `averagingSampleIndex`
#' and `splittingSampleIndex`. `averagingSampleIndex` is used to generate
#' aggregated prediction for the node. `splittingSampleIndex` is used for
#' `honestRF` which stores the splitting data when creating the tree. In
#' default, `RFNode` has the same `averagingSampleIndex` and
#' `splittingSampleIndex`.
#' @slot x A data frame or a matrix of all predictors.
#' @slot y A response vector.
#' @slot splitFeature Name of the feature that is used for splitting in this
#' node.
#' @slot splitValue The value that is used for splitting in this node.
#' @slot sampleIndex A list of index of dataset used in this node and its
#' children. `sampleIndex` contains two keys `averagingSampleIndex`
#' and `splittingSampleIndex`. `averagingSampleIndex` is used to generate
#' aggregated prediction for the node. `splittingSampleIndex` is used for
#' `honestRF` which stores the splitting data when creating the tree. In
#' default, `splittingSampleIndex` is the same as `averagingSampleIndex`.
#' @slot avgfunc An averaging function to average all the input data. The input
#' of this function should be a dataframe of predictors `x` and a vector of
#' outcome `y`. The output is a scalar. The default is to take the mean of all
#' the `y`s.
#' @slot isLeaf An indicator of whether the current node is a leaf or not.
#' @slot leftChild If the node is not a leaf node, the `leftChild` will point
#' to another node object. If it is a leaf node, the `leftChild` will be `NULL`.
#' @slot rightChild If the node is not a leaf node, the `rightChild` will point
#' to another node object. If it is a leaf node, the `rightChild` will be
#' `NULL`.
#' @slot nSplit Number of observations in the splitting dataset in this node.
#' @slot nAverage Number of observations in the averaging dataset in this node.
setClass(
  Class="RFNode",
  slots=list(
    x="data.frame",
    y="vector",
    splitFeature="numericOrNULL",
    splitValue="numericOrNULL",
    sampleIndex="list",
    avgfunc="function",
    isLeaf="logical",
    leftChild="RFNodeOrNULL",
    rightChild="RFNodeOrNULL",
    nSplit="numeric",
    nAverage="numeric"
  )
)

#' RFNode Constructor
#' @name RFNode
#' @rdname RFNode-class
#' @slot x A data frame or a matrix of all predictors.
#' @slot y A response vector.
#' @slot splitFeature Name of the feature that is used for splitting in this
#' node.
#' @slot splitValue The value that is used for splitting in this node.
#' @slot sampleIndex A list of index of dataset used in this node and its
#' children. `sampleIndex` contains two keys `averagingSampleIndex`
#' and `splittingSampleIndex`. `averagingSampleIndex` is used to generate
#' aggregated prediction for the node. `splittingSampleIndex` is used for
#' `honestRF` which stores the splitting data when creating the tree. In
#' default, `splittingSampleIndex` is the same as `averagingSampleIndex`.
#' @slot avgfunc An averaging function to average all the input data. The input
#' of this function should be a dataframe of predictors `x` and a vector of
#' outcome `y`. The output is a scalar. The default is to take the mean of all
#' the `y`s.
RFNode <- function(x, y, splitFeature, splitValue,
                   sampleIndex=list(
                     "averagingSampleIndex"=1:length(y),
                     "splittingSampleIndex"=1:length(y)
                   ),
                   avgfunc=avgMean){
  x <- as.data.frame(x)
  node <- new("RFNode",
              x=x,
              y=y,
              splitFeature=splitFeature,
              splitValue=splitValue,
              sampleIndex=sampleIndex,
              avgfunc=avgfunc,
              isLeaf=TRUE,
              leftChild=NULL,
              rightChild=NULL,
              nSplit=length(sampleIndex$splittingSampleIndex),
              nAverage=length(sampleIndex$averagingSampleIndex)
              )
  return(node)
}

######################
### Generic Method ###
######################
#' predict-RFNode
#' @name predict-RFNode
#' @rdname predict-RFNode
#' @description Return the prediction from the current node if it is a leaf
#' node, otherwise it will recursively call its children according to the
#' `splitFeature` and `splitValue`.
#' @param theObject RFNode object.
#' @param feature.new A data frame or a matrix of all testing predictors.
#' @exportMethod predict
if (!isGeneric("predict")) {
  setGeneric(
    name="predict",
    def=function(theObject, feature.new){
      standardGeneric("predict")
    }
  )
}

#' @rdname predict-RFNode
#' @aliases predict, RFNode-method
setMethod(
  f="predict",
  signature="RFNode",
  definition=function(theObject, feature.new){

    # If the node is a leaf, aggregate all its averaging data samples
    if (theObject@isLeaf) {
      return(theObject@avgfunc(
        theObject@x[theObject@sampleIndex$averagingSampleIndex, ],
        theObject@y[theObject@sampleIndex$averagingSampleIndex]))
    } else {

      # Test if the testing data have smaller feature value or bigger than the
      # current `splitFeature` and `splitValue`.
      leftIndicator <-
        feature.new[, theObject@splitFeature] < theObject@splitValue
      prediction <- rep(NA, nrow(feature.new))

      # Assign predictions from the left child
      if (sum(leftIndicator) > 0) {
        prediction[leftIndicator] <- predict(theObject@leftChild,
                                             feature.new[leftIndicator, ])
      }

      # Assign predictions from the right child
      if (sum(!leftIndicator) > 0) {
        prediction[!leftIndicator] <- predict(theObject@rightChild,
                                              feature.new[!leftIndicator, ])
      }

      # Return the prediction
      return(prediction)
    }
  }
)

#' printnode-RFNode
#' @name printnode-RFNode
#' @rdname printnode-RFNode
#' @description Print the type of the node and its prediction. If it is a tree
#' node, it will recursively print all its children.
#' @param theObject RFNode object
#' @exportMethod printnode
setGeneric(
  name="printnode",
  def=function(theObject){
    standardGeneric("printnode")
  }
)

#' @rdname printnode-RFNode
#' @aliases printnode, RFNode-method
setMethod(
  f="printnode",
  signature="RFNode",
  definition = function(theObject){
    if (theObject@isLeaf) {
      print(paste("This is a leaf with CATE:",
                  predict(theObject),
                  ", #Split:",
                  theObject@nSplit,
                  ", #Average:",
                  theObject@nAverage))
    } else {
      print(paste("This is a regular node with Split feature: ",
                  theObject@splitFeature,
                  ", Split Value:",
                  theObject@splitValue,
                  ", #Split:",
                  theObject@nSplit,
                  ", #Average:",
                  theObject@nAverage))
      printnode(theObject@leftChild)
      printnode(theObject@rightChild)
    }
  }
)
