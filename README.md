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

### Object: honestForest
#### Parameters
- `x`  
A data frame or a matrix of predictors.

- `y`    
A response vector. (regression is assumed for now)

- `ntree=500`  
Number of trees to grow. This should not be set to too small to ensure
that every input row gets predicted at least a few times.

- `mtry=max(floor(ncol(x)/3), 1)`  
Number of variables randomly sampled as candidates at each split. The default
value is set to be one third of total feature amount.

- `replace=TRUE`  
Indicator of whether sampling of cases be done with or without replacement.

- `sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x))`  
Size(s) of sample to draw.

- `splitratio = 1`  
Proportion of the data used for the splitting data set. It is a ratio between 0
and 1. If the value is one, then essentially splitting dataset becomes the total
entire sampled set and the averagin data set is empty. If it is 0 then the
splitting data set is empty and all the data is used for the averaging data set.

- `nodesize = 5`  
Minimum size of terminal nodes. Setting this number larger causes smaller trees
to be grown (and thus take less time). The default for regression is 5.

- `nodesizeavg = 5`  
Minimum size of terminal nodes for averaging dataset.

- `maxnodes=NULL`  
Maximum number of terminal nodes trees in the forest can have. If not given,
trees are grown to the maximum possible (subject to limits by nodesize).

- `splitfunc=minimizeMSE`  
A splitting function to determine the best split point among the features. The
input for the split function should be x, y, and a set of candidate features and
the its output should be the best feature and the best splitting point for that
feature. The default is to minimize the overall MSE.

- `avgfunc=avgMean`  
An averaging function to average all the observations in a node. The input of
this function should be an arbitrary vector and the output should be a scalar.
The default is to take the mean function.

#### Fields (In addition to input parameters)
- `forest`  
A list that contains the honestTree objects in the forest.

#### Functions
- constructor
Create a honestForest object by calling `grow()`.

- `addTree()`  
Add a new tree to the forest.

- `grow()`  
Call `addTree()` `ntree` amount of time to construct trees in the forest.

- `predict(x)`  
Call predict for all trees and average the predictions among all the trees.

### Object: honestTree
#### Parameters
- `x`  
(Passed as a reference from the forest)

- `y`    
(Passed as a reference from the forest)

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
(Passed as a value from the forest)

- `nodesize = 5`  
(Passed as a value from the forest)

- `nodesizeavg = 5`  
(Passed as a value from the forest)

- `maxnodes=NULL`  
(Passed as a value from the forest)

- `splitfunc=splitDeviance(splitValue, x, y)`  
(Passed as a reference from the forest)

- `avgfunc=avgMean(x, y)`  
(Passed as a reference from the tree)

#### Fields (In addition to input parameters)
- `rootNode`  
A Node object which is the root of the tree.

- 'totalNodes'
A counter of total amount of nodes in the tree.

- 'totalTerminalNodes'
A counter of total amount of terminal nodes in the tree.

#### Functions
- constructor
Create a honestTree object by calling `grow()`.

- `grow()`  
Build a decision tree regressor from the training set.

- `peak(x)`   
Return the index of the leaf Node that x falls in.

- `decision_path(x)`  
Return the decision path in the tree.

- `recusive_partition()`  
Grow the decision tree by recursively finding the best split feature and point.

- `predict(x)`  
Predict the regression value for x.

### Object: Node
#### Parameters
- `x`  
(Passed as a reference from the tree)

- `y`    
(Passed as a reference from the tree)

- `splitSampleIndex`   
Store the index of all current sampling data in this node and its children.
(Passed as a reference from the tree)

- `avgSampleIndex`  
Store the index of all current averaging data in this node and its children.
(Passed as a reference from the tree)

- `splitfunc=splitDeviance(splitValue, x, y)`  
(Passed as a reference from the forest)

- `avgfunc=avgMean(x, y)`  
(Passed as a reference from the tree)

- `splitFeature`    
The feature that is used for splitting in this node.
(Passed as a value from the tree)

- `splitValue`    
The feature value that is used for splitting in this node.
(Passed as a value from the tree)

#### Fields (In addition to input parameters)
- `leaf`  
An indicator of whether the current node is a leaf or not.

- `leftChild`    
If the node is not a leaf node, the `leftChild` will point to another
`Node` object. If it is a leaf node, the `leftChild` will be NULL.

- `rightChild`    
If the node is not a leaf node, the `rightChild` will point to another
`Node` object. If it is a leaf node, the `rightChild` will be NULL.

- `prediction`    
If the node is a leaf node, the `prediction` will be the predicted value of this
node using the passed-in function `avgfunc`.

- `splitDeviance`    
To measure the deviance of the splitting according to passed-in function
`splitfunc`.

- `nAverage`    
Number of observations from averaging dataset in this node.

- `nSplit`    
Number of observations from splitting dataset in this node.

#### Functions
- `isLeaf()`    
Test if the current node is a leaf node

- `getLeftChild()`    
Return the left child Node object.

- `getRightChild()`    
Return the right child Node object.

- `predict(x)`   
Return the prediction from the current node if it is a leaf node, otherwise it
will recursively call its children according the splitting criteria.

- `getDeviance()`   
Return the splitting deviance from the current node.

- `getSplitFeature()`   
Return the splitting feature of the current node.

- `getSplitValue()`   
Return the splitting value of the current node.

- `getSplitData()`    
Return a dataframe of all splitting dataset.

- `getAveragingData()`    
Return a dataframe of all averaging dataset.
