library(hte)

# Use Iris dataset
x <- iris[, -1]
x$Species <- as.numeric(x$Species)
y <- iris[, 1]

# Set seed for reproductivity
set.seed(24750371)

# Test selectBestFeature - one feature
selectBestFeature(
  x,
  y,
  featureList=c(1,4),
  sampleIndex=list(
    "averagingSampleIndex"=1:length(y),
    "splittingSampleIndex"=1:length(y)
  ),
  nodesize=list(
    "splittingNodeSize"=5,
    "averagingNodeSize"=5
  ),
  splitrule="variance",
  categoricalFeatureCols=list(4)
)
# $bestSplitFeature
# [1] 4
#
# $bestSplitValue
# [1] 1

# Test rcpp selectBestFeature
rcpp_selectBestFeature(
  x,
  y,
  featureList=c(1,4),
  sampleIndex=list(
    "averagingSampleIndex"=1:length(y),
    "splittingSampleIndex"=1:length(y)
  ),
  nodesize=list(
    "splittingNodeSize"=5,
    "averagingNodeSize"=5
  ),
  splitrule="variance",
  categoricalFeatureCols=list(4)
  )
# $bestSplitFeature
# [1] 4
#
# $bestSplitValue
# [1] 1

# Test selectBestFeature - two features
rcpp_selectBestFeature(
  x,
  y,
  featureList=c(1, 2),
  sampleIndex=list(
    "averagingSampleIndex"=1:length(y),
    "splittingSampleIndex"=1:length(y)
  ),
  nodesize=list(
    "splittingNodeSize"=5,
    "averagingNodeSize"=5
  ),
  splitrule="variance",
  categoricalFeatureCols=list(4)
  )
# Expected answer
# bestSplitFeature
# 2
# bestSplitValue
# 4.250153

# Test creating a tree
tree <- RFTree(
  x=x,
  y=y,
  mtry=max(floor(ncol(x)/3), 1),
  nodesize=5,
  sampleIndex=1:length(y),
  splitrule="variance",
  categoricalFeatureCols=list(4)
)

# Test showtree
showTree(tree)

# Test predict
y_pred <- predict(tree, x, x, y, function(x, y) mean(y),
                  categoricalFeatureCols=list(4))

# Mean Square Error
sum((y_pred - y)^2)
# 16.78703
