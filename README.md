# CausalRF

## Package Development in R
### Setup
This package is current in development, which means you may need to install
some development toolkits to test and improve the library. Here is a quick
tutorial on the process.

You might need to first install the following libraries:
```
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
```

To have the best development experience, you should install/update to the latest
version of [RStudio](http://www.rstudio.com/products/rstudio/download/preview/).

### R Code Workflow
To save all the editing files in the package and rebuild the package, you can
use the command
```
devtools::load_all()
```
or in RStudio you can use the shortcut `Ctrl/Cmd + Shift + L`. Once the package
is built, all the code under the folder `R/` is executed and made available
in the console.

### Documentation Workflow
Documentations are not created manually but instead in R codes. To convert all
roxygen comments in files under the folder `R/`, you can use the command
```
devtools::document()
```
or in RStudio you can use the shortcut `Ctrl/Cmd + Shift + D`. Once the
documentation is complete, you can preview documentation with `?`.

### Testing Workflow
First we need to set up `testthat`. You can use the command
```
devtools::use_testthat()
```
which creates a `tests/testthat` directory. To test all
testcases in files under the folder `testthat/`, you can use the command
```
devtools::test()
```
or in RStudio you can use the shortcut `Ctrl/Cmd + Shift + T`.

### Vignette Workflow
Vignette is used for the overall tutorial of the package. It can be created
using the following command
```
devtools::use_vignette("tutorial")
```
Modifying the file inside under the folder `vignette` and in RStudio you can
press `Ctrl/Cmd + Shift + K` to knit the vignette and preview the output.

### C++ Workflow
To integrate with C++, we need to set up Rcpp in the package. The command
```
devtools::use_rcpp()
```
will create a `src/` directory to hold all the .cpp files. Once your code is
ready to test, you can click `Build & Reload` in the build pane, or press
`Ctrl/Cmd + Shift + B`. You can also use the standard `devtools::load_all()`
process but it is more risky.

## Package Design

### Object: `honestForest`
`honestForest` object is a modified version of `randomForest`. The major change is that it aggregates a different dataset (called averaging dataset), instead of the dataset that is used for splitting (called splitting dataset). `honestForest` contains `forest` which is a list of all `honestTree` in the forest. 

#### Parameters
- `x`  
A reference to data frame or a matrix of all predictors. 

- `y`    
A reference to a response vector. 

- `ntree=500`  
Number of trees to grow. This should not be set to too small to ensure that every input row gets predicted at least a few times. The default value is 500.

- `mtry=max(floor(ncol(x)/3), 1)`  
Number of variables randomly sampled as candidates at each split. The default value is set to be one third of total feature amount.

- `replace=TRUE`  
Indicator of whether sampling of cases be done with or without replacement.

- `sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x))`  
Size(s) of sample to draw.

- `splitratio = 1`  
Proportion of the training data used as the splitting data set. It is a ratio between 0 and 1. If the ratio is 1, then essentially splitting dataset becomes the total entire sampled set and the averagin data set is empty. If the ratio is 0, then the splitting data set is empty and all the data is used for the averaging data set (This is not a good usage however since there will be no data available for splitting). 

- `nodesize = 5`  
Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). The default value is 5.

- `nodesizeavg = 5`  
Minimum size of terminal nodes for averaging dataset. The default value is 5.

- `maxnodes=NULL`  
Maximum number of terminal nodes trees in the forest can have. If not given, trees are grown to the maximum possible (subject to limits by nodesize).

- ?? `splitfunc=minimizeMSE`  
A splitting function to determine the best split point among the features. The input for the split function should be `x`, `y`, and a set of candidate features and the its output should be the best feature and the best splitting point for that feature. The default is to minimize the overall MSE.

- `avgfunc=avgMean`  
An averaging function to average all the input data. The input of this function should be a dataframe of predictors `x` and a vector of outcome `y`. The output is a scalar. The default is to take the mean of all the `y`s.

#### Fields (In addition to input parameters)
- `forest`  
A list of `honestTree` in the forest.

#### Functions
- constructor	
Create a `honestForest` object by calling `grow()`.

- `addTree()`  
Create a new `honestTree` and add it to the `forest`.

- `grow()`  
Call `addTree()` `ntree` amount of time to construct trees in the forest.

- `predict(x)`  
Call predict for all trees and average the predictions among all the trees.


### Object: `honestTree`
`honestTree` is the unit component in the `honestTree` which composes `honestNode`. The tree uses recursively partitioning to determine the best `splitFeature` and `splitValue` for each level, and recursively partition the dataset until it reaches the limitation of `nodesize` or `maxnodes`.

#### Parameters
- `x`  
A reference to data frame or a matrix of all predictors. 

- `y`    
A reference to a response vector. 

- `splitSampleIndex`   
A list of index that records observations that are used in splitting dataset.
The index are directed linked to the original dataset `x` and `y` from forest.
Essentially, given this list, we can generate the splitting dataset.

- `avgSampleIndex`  
A list of index that records observations that are used in averaging dataset.
The index are directed linked to the original dataset `x` and `y` from forest.
Essentially, given this list, we can generate the averaging dataset. If it is
not given, it will be the same as `splitSampleIndex`.

- `mtry=max(floor(ncol(x)/3), 1)`  
Number of variables randomly sampled as candidates at each split. The default value is set to be one third of total feature amount.

- `nodesize = 5`  
Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). The default value is 5.

- `nodesizeavg = 5`  
Minimum size of terminal nodes for averaging dataset. The default value is 5.

- `maxnodes=NULL`  
Maximum number of terminal nodes trees in the forest can have. If not given, trees are grown to the maximum possible (subject to limits by nodesize).

- ?? `splitfunc=minimizeMSE`  
A splitting function to determine the best split point among the features. The input for the split function should be `x`, `y`, and a set of candidate features and the its output should be the best feature and the best splitting point for that feature. The default is to minimize the overall MSE.

- `avgfunc=avgMean`  
An averaging function to average all the input data. The input of this function should be a dataframe of predictors `x` and a vector of outcome `y`. The output is a scalar. The default is to take the mean of all the `y`s.

#### Fields (In addition to input parameters)
- `rootNode`  
A `honestNode` object which is the root of the tree.

- `totalNodes`	
A counter of total amount of nodes in the tree.

- `totalTerminalNodes`	
A counter of total amount of terminal nodes in the tree.

#### Functions
- constructor	
Create a `honestTree` object by calling `grow()`.

- `grow()`  
Build a decision tree regressor using `recusive_partition()`.

- `recusive_partition()`  
Grow the decision tree by recursively finding the best split feature and point.

- `peak(x)`   
Return the leaf `honestNode` that `x` falls in.

- `decision_path(x)`  
Return the decision path in the tree.

- `predict(x)`  
Predict the regression value for `x` by calling `predict(x)` from the root `honestNode`.

### Object: `honestNode`
`honestNode` is the basic element inside a `honestTree`. For each node, it contains the splitting and averaging dataset that are assigned to the node. The `honestNode` can be either a leaf and a tree node (non-leaf). If it is a leaf node, `avgfunc` can be called to aggregate all the averaging observations in the node and return a prediction for the node. If it is a tree node, it will contains `leftChild` and `rightChild` which will be another two `honestNode`. The current parent node can be used to track the `splitFeature` and `splitValue`. 

#### Parameters
- `x`  
A reference to data frame or a matrix of all predictors. 

- `y`    
A reference to a response vector. 

- `splitSampleIndex`   
Index of splitting dataset used in this node and its children. `x[splitSampleIndex]` will return a dataframe of predictors in the splitting dataset.

- `avgSampleIndex=splitSampleIndex`  
Index of averaging dataset used in this node and its children. `x[avgSampleIndex]` will return a dataframe of predictors in the averaging dataset. If the `avgSampleIndex` is `NULL`, it will be set the same as `splitSampleIndex`.

- `avgfunc=avgMean`  
An averaging function to average all the input data. The input of this function should be a dataframe of predictors `x` and a vector of outcome `y`. The output is a scalar. The default is to take the mean of all the `y`s.

- `splitFeature`    
Name of the feature that is used for splitting in this node.

- `splitValue`    
The value that is used for splitting in this node.

#### Fields (In addition to input parameters)
- `isLeaf`  
An indicator of whether the current node is a leaf or not.

- `leftChild`    
If the node is not a leaf node, the `leftChild` will point to another `honestNode` object. If it is a leaf node, the `leftChild` will be `NULL`.

- `rightChild`    
If the node is not a leaf node, the `rightChild` will point to another `honestNode` object. If it is a leaf node, the `rightChild` will be `NULL`.

- `prediction`    
If the node is a leaf node, the `prediction` will be the predicted value of this node using the passed-in function `avgfunc`. If the node is a not a leaf node, `prediction` will be `NULL`.

- `splitVariance`    
To measure the variance of the splitting.

- `nAverage`    
Number of observations in the averaging dataset in this node.

- `nSplit`    
Number of observations in the splitting dataset in this node.

#### Functions
- constructor	
Create a `honestNode` object using the input parameters.

- `isLeaf()`    
Test if the current node is a leaf node

- `setLeftChild(node)`    
Set the `leftChild` to be `node`.

- `setRightChild(node)`    
Set the `rightChild` to be `node`.

- `getLeftChild()`    
Return `leftChild`.

- `getRightChild()`    
Return `rightChild`.

- `predict(x)`   
Return the prediction from the current node if it is a leaf node, otherwise it will recursively call its children according to the `splitFeature` and `splitValue`.

- `getsplitVariance()`   
Return `splitVariance`.

- `getSplitFeature()`   
Return `splitFeature`.

- `getSplitValue()`   
Return `splitValue`.

- `getSplitData()`    
Return splitting dataset. It contains both predictors and responses, namely `x[splitSampleIndex]` and `y[splitSampleIndex]`.

- `getAveragingData()`    
Return averaging dataset. It contains both predictors and responses, namely `x[avgSampleIndex]` and `y[avgSampleIndex]`.
