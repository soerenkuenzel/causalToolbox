# Use Iris dataset
x <- iris[, -1]
x$Species <- as.numeric(x$Species)
y <- iris[, 1]

# Set seed for reproductivity
set.seed(24750371)

# Test selectBestFeature - one feature
selectBestFeature(x, y, featureList=c(1),
                  sampleIndex=list(
                    "averagingSampleIndex"=1:length(y),
                    "splittingSampleIndex"=1:length(y)
                  ),
                  nodesize=list(
                    "splittingNodeSize"=5,
                    "averagingNodeSize"=5
                  ),
                  splitrule="variance")
# Expected answer
# bestSplitFeature
# 1
# bestSplitValue
# 3.375623

# Test selectBestFeature - two features
selectBestFeature(x, y, featureList=c(1, 2),
                  sampleIndex=list(
                    "averagingSampleIndex"=1:length(y),
                    "splittingSampleIndex"=1:length(y)
                  ),
                  nodesize=list(
                    "splittingNodeSize"=5,
                    "averagingNodeSize"=5
                  ),
                  splitrule="variance")
# Expected answer
# bestSplitFeature
# 2
# bestSplitValue
# 4.295773

# Test creating a tree
tree <- RFTree(
  x=x,
  y=y,
  mtry=max(floor(ncol(x)/3), 1),
  nodesize=5,
  sampleIndex=1:length(y),
  splitrule="variance"
)

# Test showtree
showTree(tree)

# Test predict
y_pred <- predict(tree, x, x, y, avgMean)

# Mean Square Error
sum((y_pred - y)^2)
# 16.34923
