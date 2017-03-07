library(hte)

# Use Iris dataset
x <- iris[, -1]
x$Species <- as.numeric(x$Species)
y <- iris[, 1]

# A zoombie split
sampleIndex <- 1:length(y)
partitionLeftIndex <- x$Sepal.Width < 3.1
leftIndex <- sampleIndex[partitionLeftIndex]
rightIndex <- sampleIndex[!partitionLeftIndex]

# Create two leaf node
leftChild <- RFNode(
  sampleIndex=list(
    "averagingSampleIndex"=leftIndex,
    "splittingSampleIndex"=leftIndex
  ),
  splitFeature=numeric(),
  splitValue=numeric(),
  child=list()
)

rightChild <- RFNode(
  sampleIndex=list(
    "averagingSampleIndex"=rightIndex,
    "splittingSampleIndex"=rightIndex
  ),
  splitFeature=numeric(),
  splitValue=numeric(),
  child=list()
)

# Create a root object
root <- RFNode(
  sampleIndex=list(
    "averagingSampleIndex"=sampleIndex,
    "splittingSampleIndex"=sampleIndex
  ),
  splitFeature=1,
  splitValue=3.1,
  child=list(
    "leftChild"=leftChild,
    "rightChild"=rightChild
  )
)

# Test print leaf node
printnode(leftChild)
printnode(rightChild)

# Test print tree node
printnode(root)

# Test predict leaf node
predict(leftChild, x, x, y, function(x, y) mean(y))
predict(rightChild, x, x, y, function(x, y) mean(y))

# Test predict tree node
predict(root, x, x, y, function(x, y) mean(y))
