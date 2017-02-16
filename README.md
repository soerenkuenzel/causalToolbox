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

### 1. Random forest and its variations

#### `RF`
`RF` object is an object implementing the most basic version of a random forest.

##### Parameters
- `x`  
A reference to data frame or a matrix of all predictors.

- `y`    
A reference to a response vector.

- `ntree=500`  
Number of trees to grow. This should not be set to too small to ensure that
every input row gets predicted at least a few times. The default value is 500.

- `mtry=max(floor(ncol(x)/3), 1)`  
Number of variables randomly sampled as candidates at each split. The default
alue is set to be one third of total feature amount.

- `replace=TRUE`  
Indicator of whether sampling of cases be done with or without replacement.

- `sampsize=if (replace) nrow(x) else ceiling(.632*nrow(x))`  
Size(s) of sample to draw.

- `nodesize=5`  
Minimum size of terminal nodes. Setting this number larger causes smaller trees
to be grown (and thus take less time). The default value is 5.

- `maxnodes=NULL`  
Maximum number of terminal nodes trees in the forest can have. If not given,
trees are grown to the maximum possible (subject to limits by nodesize).

- `splitrule='variance'`  
A splitting rule to determine the best split point among the features. There
are two possible split rules in the package, `variance` and `maxstats`. The
default is `variance` to minimize the overall MSE.

- `avgfunc=avgMean`  
An averaging function to average all the input data. The input of this function
should be a dataframe of predictors `x` and a vector of outcome `y`. The output
is a scalar. The default is to take the mean of all the `y`s.

##### Fields (In addition to parameters)
- `forest`  
A list of `RFTree` in the forest. If the class is extended, the list may contain
 the corresponding extended `RFTree` object.

##### Functions
- constructor
Create a `RF` object by calling `grow()`.

- `addTree()`  
Create a new `RFTree` and add it to the `forest`.

- `grow()`  
Call `addTree()` `ntree` amount of time to construct trees in the forest.

- `predict(x)`  
Call predict for all trees and average the predictions among all the trees.

----

#### `honestRF`
`honestRF` inherits `RF`, which serves as a modified version of `RF`. The major
change is that it trains honest trees instead of adaptive trees. Adaptive trees
are the original trees as they were used in the original
implementation and honest trees differ in that they require two data sets to do
tree estimation. One data set is used to create the trees and the other one is
used to receive the leaf estimates.  

##### Parameters (In addition to those inherited from `RF`)
- `splitratio = 1`  
Proportion of the training data used as the splitting data set. It is a ratio between 0 and 1. If the ratio is 1, then essentially splitting dataset becomes the total entire sampled set and the averagin data set is empty. If the ratio is 0, then the splitting data set is empty and all the data is used for the averaging data set (This is not a good usage however since there will be no data available for splitting).

- `nodesizeAvg=5`  	
Minimum size of terminal nodes for averaging dataset. The default value is 5.

##### Functions (Overwritten)
- constructor
Create a `honestRF` object by calling `grow()`.

- `addTree()`  
Create a new `honestRFTree` and add it to the `forest`.


### 2. Tree and its variations

#### `RFTree`
`RFTree` is the unit component in the `RF` which composes `RFNode`. The tree uses recursively partitioning to determine the best `splitFeature` and `splitValue` for each level, and recursively split the dataset until it reaches the limitation according to `nodesize` or `maxnodes`.

##### Parameters
- `x`  
A reference to data frame or a matrix of all predictors.

- `y`    
A reference to a response vector.

- `splitSampleIndex`   
A list of the index of observations that are used in training. The index are based on the original dataset `x` and `y` from forest. Essentially, `x[splitSampleIndex]` generates the whole training dataset.

- `mtry=max(floor(ncol(x)/3), 1)`  
Number of variables randomly sampled as candidates at each split. The default value is set to be one third of total feature amount.

- `nodesize=5`  
Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). The default value is 5.

- `nodesizeavg=5`  
Minimum size of terminal nodes for averaging dataset. The default value is 5.

- `maxnodes=NULL`  
Maximum number of terminal nodes trees in the forest can have. If not given, trees are grown to the maximum possible (subject to limits by nodesize).

- `splitrule='variance'`  
A splitting rule to determine the best split point among the features. There are two possible split rules in the package, `variance` and `maxstats`. The default is `variance` to minimize the overall MSE.

- `avgfunc=avgMean`  
An averaging function to average all the input data. The input of this function should be a dataframe of predictors `x` and a vector of outcome `y`. The output is a scalar. The default is to take the mean of all the `y`s.

##### Fields (In addition to parameters)
- `root`  
A `RFNode` object which is the root of the tree. If the class is extended, the list may contain the corresponding extended `RFNode` object.

- `totalNodes`
A counter of total amount of nodes in the tree.

- `totalTerminalNodes`
A counter of total amount of terminal nodes in the tree.

##### Functions
- constructor
Create a `RFTree` object by calling `recusive_partition()`.

- `validSplitTest()`
Determine if a split is valid. If a split is valid, it must have at least `minSizeSplit` observations in the training dataset.

- `selectBestFeature()`
Find the best split value for all features. The `splitFeature` and its corresponding `splitValue` minimizes the specified `splitrule`.

- `recusivePartition()`  
Grow the decision tree by recursively finding the best split feature and value.

- `peak(x)`   
Return the leaf node that `x` falls in.

- `decisionPath(x)`  
Return the decision path in the tree.

- `predict(x)`  
Predict the regression value for `x` by calling `predict(x)` from the root.

----

#### `honestRFTree`
`honestRFTree` inherits `RFTree`, which serves as a modified version of `RFTree`. The major change is that when `honestRFTree` determines a valid split point, it looks at whether or not it is valid for both averaging and splitting dataset.

##### Parameters (In addition to those inherited from `RFTree`)
- `avgSampleIndex=splitSampleIndex`  
A list of the index of observations that are used as averaging dataset. The index are based on the original dataset `x` and `y` from forest. Essentially, `x[avgSampleIndex]` generates the whole splitting dataset. The default case is `avgSampleIndex=splitSampleIndex` where the `honestRFTree` is essentially a normal `RFTree`.

- `nodesizeAvg=5`  	
Minimum size of terminal nodes for averaging dataset. The default value is 5.

##### Functions (Overwritten)
- constructor
Create a `honestRFTree` object by calling `recusive_partition()`.

- `validSplitTest()`		
Determine if a split is valid. If a split is valid, it must have at least both `nodesize` observations in the training (splitting) dataset and `nodesizeAvg` observations in the averaging dataset.


### 3. Node and its variations

#### `RFNode`
`RFNode` is the basic element inside a `honestRFTree`. For each node, it contains the averaging dataset that are assigned to the node. The `RFNode` can be either a leaf and a tree node (non-leaf). If it is a leaf node, `avgfunc` can be called to aggregate all the observations in the node and return a prediction for the node. If it is a tree node, it will contains `leftChild` and `rightChild` which will be another two `RFNode`. The current parent node can be used to track the `splitFeature` and `splitValue`.

##### Parameters
- `x`  
A reference to data frame or a matrix of all predictors.

- `y`    
A reference to a response vector.

- `splitSampleIndex`   
Index of splitting dataset used in this node and its children. `x[splitSampleIndex]` will return a dataframe of predictors in the splitting dataset.

- `avgfunc=avgMean`  
An averaging function to average all the input data. The input of this function should be a dataframe of predictors `x` and a vector of outcome `y`. The output is a scalar. The default is to take the mean of all the `y`s.

- `splitFeature`    
Name of the feature that is used for splitting in this node.

- `splitValue`    
The value that is used for splitting in this node.

##### Fields (In addition to input parameters)
- `isLeaf`  
An indicator of whether the current node is a leaf or not.

- `leftChild`    
If the node is not a leaf node, the `leftChild` will point to another node object. If it is a leaf node, the `leftChild` will be `NULL`.

- `rightChild`    
If the node is not a leaf node, the `rightChild` will point to another node object. If it is a leaf node, the `rightChild` will be `NULL`.

- `prediction`    
If the node is a leaf node, the `prediction` will be the predicted value of this node using the passed-in function `avgfunc`. If the node is a not a leaf node, `prediction` will be `NULL`.

- `splitVariance`    
To measure the variance of the splitting.

- `n`    
Number of observations in the dataset in this node.

#### Functions
- constructor
Create a `RFNode` object using the input parameters.

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

- `getAveragingData()`    
Return averaging dataset. It contains both predictors and responses, namely `x[avgSampleIndex]` and `y[avgSampleIndex]`.

----


#### `honestRFNode`
`honestRFNode` inherits `RFNode`, which serves as a modified version of `RFNode`. The major change is that when `honestRFNode` also stores the splitting dataset in the node in addition to the averaging dataset.

##### Parameters (In addition to those inherited from `RFNode`)
- `splitSampleIndex=avgSampleIndex`   
Index of splitting dataset used in this node and its children. `x[splitSampleIndex]` will return a dataframe of predictors in the splitting dataset. The default case is `splitSampleIndex=avgSampleIndex`.

##### Fields (In addition to those inherited from `RFNode`)
- `nSplit`    
Number of observations in the splitting dataset in this node.

##### Functions (Overwritten)
- constructor
Create a `honestRFTree` object by calling `recusive_partition()`.

- `getSplitData()`    
Return splitting dataset. It contains both predictors and responses, namely `x[splitSampleIndex]` and `y[splitSampleIndex]`.
