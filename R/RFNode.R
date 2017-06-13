######################
### avgMean Method ###
######################
#' @title avgMean
#' @description A default function to average the observations in each tree leaf
#' by taking the mean of the outcomes.
#' @param x A dataframe of observations in the current tree leaf
#' @param y A vector of outcomes in the current tree leaf
#' @return The averaging metrics of the data
avgMean <- function(x, y, y_sd = NULL) {
  return(mean(y))
}

#' @title weightedavgMean
#' @description A default function to average the observations in each tree leaf
#' by taking a weighted mean according with minimal variance.
#' @param x A dataframe of observations in the current tree leaf
#' @param y A vector of outcomes in the current tree leaf
#' @param y_sd A vector. The ith position contains the standard deviation of Y_i
#' @return The weighted averaging of Y
weightedavgMean <- function(x, y, y_sd) {
  normalizer <- sum(1 / y_sd^2)
  return(sum(y / y_sd ^ 2) / normalizer)
}


######################################
### Random Forest Node Constructor ###
######################################
#' @title RFNode Constructor
#' @name RFNode-class
#' @rdname RFNode-class
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
#' @exportClass RFNode
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

#' @title RFNode Constructor
#' @name RFNode
#' @rdname RFNode-class
#' @description A constructor to create a RFNode
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
#' @export RFNode
setGeneric(
  name="RFNode",
  def=function(
    sampleIndex,
    splitFeature,
    splitValue,
    child
  ){
    standardGeneric("RFNode")
  }
)

#' @title RFNode Constructor
#' @rdname RFNode-RFNode
#' @aliases RFNode, RFNode-method
#' @return a `RFNode` object
RFNode <- function(sampleIndex = list("averagingSampleIndex" = vector(),
                                      "splittingSampleIndex" = vector()),
                   splitFeature = numeric(),
                   splitValue = numeric(),
                   child = list()) {


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
    node <- new(
      "RFNode",
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
### Predict Method ###
######################
#' predict-RFNode
#' @name predict-RFNode
#' @rdname predict-RFNode
#' @description Return the prediction from the current node if it is a leaf
#' node, otherwise it will recursively call its children according to the
#' `splitFeature` and `splitValue`.
#' @param object A `RFNode`` object.
#' @param feature.new A data frame of testing predictors.
#' @param x A data frame of all training predictors.
#' @param y A vector of all training responses.
#' @param avgfunc An averaging function to average observations in the node. The
#' function is used for prediction. The input of this function should be a
#' dataframe of predictors `x` and a vector of outcomes `y`. The output is a
#' scalar. The default function is to take the mean of vector `y`.
#' @param categoricalFeatureCols A list of index for all categorical data. Used
#' for trees to detect categorical columns.
#' @return A vector of predicted responses.
#' @aliases predict, RFNode-method
#' @exportMethod predict
setMethod(
  f="predict",
  signature="RFNode",
  definition=function(
    object,
    feature.new,
    x,
    y,
    se = NULL,
    avgfunc,
    categoricalFeatureCols
    ){
    # If the node is a leaf, aggregate all its averaging data samples
    if (length(object@child) == 0) {
      predicted_value <- avgfunc(
        x[object@sampleIndex$averagingSampleIndex, ],
        y[object@sampleIndex$averagingSampleIndex],
        se[object@sampleIndex$averagingSampleIndex]
        )
      prediction <- rep(predicted_value, nrow(feature.new))
      return(prediction)

    } else {

      # Test if the splitting feature is categorical
      if (object@splitFeature %in% categoricalFeatureCols){
        leftIndicator <-
          feature.new[, object@splitFeature] == object@splitValue
      } else {
        # For regression, split left and right according to the split point
        leftIndicator <-
          feature.new[, object@splitFeature] < object@splitValue
      }

      # Initialize a matrix for predictions
      prediction <- rep(NA, nrow(feature.new))

      # Assign predictions from the left child
      if (sum(leftIndicator) > 0) {
        prediction[leftIndicator] <- predict(
          object@child$leftChild,
          feature.new[leftIndicator, ],
          x, y, se, avgfunc, categoricalFeatureCols
          )
      }

      # Assign predictions from the right child
      if (sum(!leftIndicator) > 0) {
        prediction[!leftIndicator] <- predict(
          object@child$rightChild,
          feature.new[!leftIndicator, ],
          x, y, se, avgfunc, categoricalFeatureCols
          )
      }

      # Return the prediction
      return(prediction)
    }
  }
)

########################
### printnode Method ###
########################
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
