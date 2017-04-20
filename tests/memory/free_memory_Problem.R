################################################################################
# This file is here to reproduce the memory problem ############################
################################################################################



start_time <- Sys.time()
n <- 10000
p <- 10
x <- as.data.frame(matrix(rnorm(n*p), nrow = n))
y <- rnorm(n)
testForest <- honestRFRcpp(x, y, verbose=FALSE)
end_time <- Sys.time()
end_time - start_time




