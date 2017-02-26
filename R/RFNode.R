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
#' `RFNode` can be either a leaf or a tree node (non-leaf). If it is a leaf
#' node, `avgfunc` can be called to aggregate all the observations in the node
#' and save a prediction for the node. If it is a tree node, it will contains
#' `leftChild` and `rightChild` which will be another two `RFNode`s. The parent
#' node can be used to track the `splitFeature` and `splitValue`. Please note
#' that in the `RFNode`, `sampleIndex` essentially tells what data reside
#' in the node. Every node contains two `sampleIndex`: `averagingSampleIndex`
#' and `splittingSampleIndex`. `averagingSampleIndex` is used to generate
#' aggregated prediction for the node. `splittingSampleIndex` is used for
#' `honestRF` which stores the splitting data when creating the tree. In
#' default, `RFNode` has the same `averagingSampleIndex` and
#' `splittingSampleIndex`.
#' @slot sampleIndex A list of index of dataset used in this node and its
#' children. `sampleIndex` contains two keys `averagingSampleIndex`
#' and `splittingSampleIndex`. `averagingSampleIndex` is used to generate
#' aggregated prediction for the node. `splittingSampleIndex` is used for
#' `honestRF` which stores the splitting data when creating the tree. In
#' default, `splittingSampleIndex` is the same as `averagingSampleIndex`.
#' @slot splitFeature Name of the feature that is used for splitting in this
#' node.
#' @slot splitValue The value that is used for splitting in this node.
#' @slot child If the node is not a leaf node, the `child` will be a list of two
#' `RFNode` objects. If it is a leaf node, the `child` will be `NULL`.
#' @slot nSplit Number of observations in the splitting dataset in this node.
#' @slot nAverage Number of observations in the averaging dataset in this node.
setClass(
  Class="RFNode",
  slots=list(
    sampleIndex="list",
    splitFeature="numeric",
    splitValue="numeric",
    child="list",
    nSplit="numeric",
    nAverage="numeric"
  )
)

#' RFNode Constructor
#' @name RFNode
#' @rdname RFNode-class
#' @param sampleIndex A list of index of dataset used in this node and its
#' children. `sampleIndex` contains two keys `averagingSampleIndex`
#' and `splittingSampleIndex`. `averagingSampleIndex` is used to generate
#' aggregated prediction for the node. `splittingSampleIndex` is used for
#' `honestRF` which stores the splitting data when creating the tree. In
#' default, `splittingSampleIndex` is the same as `averagingSampleIndex`.
#' @param splitFeature Name of the feature that is used for splitting in this
#' node.
#' @param splitValue The value that is used for splitting in this node.
#' @param child If the node is not a leaf node, the `child` will be a list of
#' two `RFNode` objects. If it is a leaf node, the `child` will be `NULL`.
#' @return a `RFNode` object
RFNode <- function(
  sampleIndex=list(
    "averagingSampleIndex"=vector(),
    "splittingSampleIndex"=vector()
  ),
  splitFeature=numeric(),
  splitValue=numeric(),
  child=list()
  ){

  if (length(child) == 0) {
    # Both children are null, create a leaf node
    node <- new("RFNode",
                sampleIndex=sampleIndex,
                splitFeature=numeric(),
                splitValue=numeric(),
                child=list(),
                nSplit=length(sampleIndex$splittingSampleIndex),
                nAverage=length(sampleIndex$averagingSampleIndex)
    )
  } else {
    # Create a tree node
    node <- new("RFNode",
                sampleIndex=list(),
                splitFeature=splitFeature,
                splitValue=splitValue,
                child=child,
                nSplit=numeric(),
                nAverage=numeric()
    )
  }
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
#' @param object RFNode object.
#' @param feature.new A data frame or a matrix of all testing predictors.
#' @param x A data frame or a matrix of all training predictors.
#' @param y A vector of all training responses.
#' @param avgfunc An averaging function to average all the input data. The input
#' of this function should be a dataframe of predictors `x` and a vector of
#' outcome `y`. The output is a scalar. The default is to take the mean of all
#' the `y`s.
#' @return A vector of predicted responses.
#' @aliases predict, RFNode-method
setMethod(
  f="predict",
  signature="RFNode",
  definition=function(object, feature.new, x, y, avgfunc){

    # If the node is a leaf, aggregate all its averaging data samples
    if (length(object@child) == 0) {
      predicted_value <- avgfunc(
        x[object@sampleIndex$averagingSampleIndex, ],
        y[object@sampleIndex$averagingSampleIndex]
        )
      prediction <- rep(predicted_value, nrow(feature.new))
      return(prediction)

    } else {
      # Test if the testing data have smaller feature value or bigger than the
      # current `splitFeature` and `splitValue`.
      leftIndicator <-
        feature.new[, object@splitFeature] < object@splitValue

      # Initialize a matrix for predictions
      prediction <- rep(NA, nrow(feature.new))

      # Assign predictions from the left child
      if (sum(leftIndicator) > 0) {
        prediction[leftIndicator] <- predict(object@child$leftChild,
                                             feature.new[leftIndicator, ],
                                             x, y, avgfunc)
      }

      # Assign predictions from the right child
      if (sum(!leftIndicator) > 0) {
        prediction[!leftIndicator] <- predict(object@child$rightChild,
                                              feature.new[!leftIndicator, ],
                                              x, y, avgfunc)
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
#' @param object RFNode object
#' @param indentSpace The indentation space of the current node in the standard
#' output. The initial value is 0. Once the tree grows, it increments by 2.
#' @exportMethod printnode
setGeneric(
  name="printnode",
  def=function(object, indentSpace=0){
    standardGeneric("printnode")
  }
)

#' @rdname printnode-RFNode
#' @aliases printnode, RFNode-method
setMethod(
  f="printnode",
  signature="RFNode",
  definition = function(object, indentSpace=0){
    if (length(object@child) == 0) {
      # Leaf node
      print(
        paste(
          paste(rep(' ', indentSpace), collapse=''),
          "Leaf Node:",
          "# of Split =",
          object@nSplit,
          ", # of Average =",
          object@nAverage
          )
        )
    } else {
      # Tree node
      print(
        paste(
          paste(rep(' ', indentSpace), collapse=''),
          "Tree Node:",
          "Split feature =",
          object@splitFeature,
          ", Split Value =",
          object@splitValue
        )
      )
      printnode(object@child$leftChild, indentSpace+2)
      printnode(object@child$rightChild, indentSpace+2)
    }
  }
)
