library(MASS)
nfeatures <- c(3, 5, 20, 100, 1000)
nrows <- c(20, 100, 500, 1000, 10000)
randomForestResult <- matrix(numeric(), nrow=length(nrows), ncol=length(nfeatures))
RFResult <- matrix(numeric(), nrow=length(nrows), ncol=length(nfeatures))

library(randomForest)
library(causalRF)

for (p in 1:length(nfeatures)){
  for (n in 1:length(nrows)){
    x <- data.frame(replicate(nfeatures[p], sample(0:10, nrows[n], rep=TRUE)))
    y <- rowMeans(x) + rnorm(nrows[n])
    start_time <- Sys.time()
    rf_brei <- randomForest(x, y)
    randomForestResult[[n, p]] <- Sys.time() - start_time
    start_time <- Sys.time()
    rf_our <- RF(x, y, nthread=10)
    RFResult[[n, p]] <- Sys.time() - start_time
  }
}

write.matrix(randomForestResult, "randomForestResult")
write.matrix(RFResult, "RFResult")
