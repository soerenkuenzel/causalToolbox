# Use Iris dataset
x <- iris[, -1]
x$Species <- as.numeric(x$Species)
y <- iris[, 1]

# Set seed for reproductivity
set.seed(24750371)

# Test creating a honest RFTree (mimic RFTree)
tree <- honestRFTree(
  x=x,
  y=y,
  mtry=max(floor(ncol(x)/3), 1),
  nodesize=list(
    "averagingNodeSize"=5,
    "splittingNodeSize"=5
  ),
  sampleIndex=list(
    "averagingSampleIndex"=1:length(y),
    "splittingSampleIndex"=1:length(y)
  ),
  splitrule="variance"
)

# Test showtree
showTree(tree)

# Test predict
y_pred <- predict(tree, x, x, y, avgMean)

# Mean Square Error
sum((y_pred - y)^2)
# 31.93993

# Test creating a honest RFTree (half split, half averaging)
tree <- honestRFTree(
  x=x,
  y=y,
  mtry=3,
  nodesize=list(
    "averagingNodeSize"=3,
    "splittingNodeSize"=3
  ),
  sampleIndex=list(
    "averagingSampleIndex"=1:(length(y)/2),
    "splittingSampleIndex"=(length(y)/2+1):length(y)
  ),
  splitrule="variance"
)

# Test showtree
showTree(tree)

# Test predict
y_pred <- predict(tree, x, x, y, avgMean)

# Mean Square Error
sum((y_pred - y)^2)
# 34.47635
