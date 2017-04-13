library(hte)
experiment <- simulate_causal_experiment(
  ntrain = 10000,
  ntest = 10000,
  dim = 20,
  alpha = 0.01,
  feat_distribution = "normal",
  setup = "RespSparseTau1strong",
  testseed = 293901,
  trainseed = 93007
)


yobs_1 <- experiment$Yobs_tr[experiment$W_tr == 1]
X_1 <- experiment$feat_tr[experiment$W_tr == 1, ]

set.seed(78136)

m_1 <-
  honestRF(
    x = X_1,
    y = yobs_1,
    ntree = 500,
    replace = TRUE,
    sampsize = round(0.1 * length(yobs_1)),
    mtry = 13,
    nodesizeSpl = 30,
    nthread = 4,
    splitrule =  'variance',
    splitratio = 0.5,
    nodesizeAvg = 1
  )
