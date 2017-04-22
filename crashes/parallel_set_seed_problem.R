# weird phenomena: When we parallelize then there is still a little bit of noise
# happening because of it. When we choose nthread = 1 and don't parallelize,
# then this does not occur.







r1c <- r1t <- r2c <- r2t <- rp <- NULL
for(i in 1:5){
  set.seed(432)
  cate_problem <-
    simulate_causal_experiment(
      ntrain = 400,
      ntest = 100,
      dim = 20,
      alpha = .1,
      feat_distribution = "normal",
      setup = "RespSparseTau1strong",
      testseed = 543,
      trainseed = 234
    )

  xl <- X_RF(feat = cate_problem$feat_tr,
             yobs = cate_problem$Yobs_tr,
             tr = cate_problem$W_tr,
             nthread = 4,
             verbose = TRUE)

  r1c <- c(r1c, predict(xl@base_learners[["l_first_0"]], cate_problem$feat_te)[1])
  r1t <- c(r1t, predict(xl@base_learners[["l_first_1"]], cate_problem$feat_te)[1])
  r2c <- c(r2c, predict(xl@base_learners[["l_second_0"]], cate_problem$feat_te)[1])
  r2t <- c(r2t, predict(xl@base_learners[["l_second_1"]], cate_problem$feat_te)[1])
  rp <- c(rp, predict(xl@base_learners[["l_prop"]], cate_problem$feat_te)[1])
}
cbind(r1c, r1t, r2c, r2t, rp)

