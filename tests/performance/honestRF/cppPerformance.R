library(MASS)
nfeatures <- c(3, 5, 20)
nrows <- c(20, 100, 500, 1000, 10000)
randomForestResult <- matrix(numeric(), nrow=length(nrows), ncol=length(nfeatures))
rangerResult <- matrix(numeric(), nrow=length(nrows), ncol=length(nfeatures))
# honestRFResult <- matrix(numeric(), nrow=length(nrows), ncol=length(nfeatures))
honestRFCppResult <- matrix(numeric(), nrow=length(nrows), ncol=length(nfeatures))

library(ranger)
library(randomForest)
library(hte)

for (p in 1:length(nfeatures)){
  for (n in 1:length(nrows)){
    x <- data.frame(replicate(nfeatures[p], sample(0:10, nrows[n], rep=TRUE)))
    y <- rowMeans(x) + rnorm(nrows[n])
    start_time <- Sys.time()
    rf_brei <- randomForest(x, y)
    randomForestResult[[n, p]] <- difftime(Sys.time(), start_time, units='secs')
    start_time <- Sys.time()
    # rf_our <- RF(x, y, nthread=10)
    # honestRFResult[[n, p]] <- difftime(Sys.time(), start_time, units='secs')
    # start_time <- Sys.time()
    rf_cpp <- honestRFRcpp(x, y)
    honestRFCppResult[[n, p]] <- difftime(Sys.time(), start_time, units='secs')
    start_time <- Sys.time()
    rf_ranger <- ranger(y ~. ,data=data.frame(x,y))
    rangerResult[[n, p]] <- difftime(Sys.time(), start_time, units='secs')
    }
}

# write.matrix(randomForestResult, "randomForestResult")
# write.matrix(RFResult, "RFResult")
#
# RF_result = read.table("tests/performance/RFResult")
# randomForest_result = read.table("tests/performance/randomForestResult")

library(ggplot2)

for (i in 1:length(nfeatures)){
  df <- data.frame(
    nrows=nrows,
    # honestRFResult[,i],
    hte=honestRFCppResult[,i],
    ranger=rangerResult[, i],
    randomForest=randomForestResult[,i]
    )

  colnames(df) <- c(
    "nrows",
    "honestRF_Rcpp",
    "ranger",
    "randomForest")

  ggplot(df,
    aes(x=nrows)) +
    # geom_line(aes(y=honestRF_R, fill="honestRF_R"), colour="hte_R") +
    geom_line(aes(y=honestRF_Rcpp, colour="hte"))  +
    geom_line(aes(y=ranger, colour="ranger")) +
    geom_line(aes(y=randomForest, colour="randomForest"))  +
    scale_colour_manual(values = c(
      "hte" = "red",
      "ranger" = "green",
      "randomForest" = "blue")) +
    xlab("nrows") +
    ylab("running time (seconds)")+
    ggtitle(paste("Comparision when total features = ", nfeatures[i])) +
    theme_classic()

  ggsave(paste("tests/performance/nfeatures", nfeatures[i], ".pdf", sep=""), width = 13, height = 8)
}
