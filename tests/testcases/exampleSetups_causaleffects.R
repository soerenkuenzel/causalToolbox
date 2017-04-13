library(hte)
simulate_cormat(4, 0)
cov(simulate_causal_experiment(ntrain = 1000000, ntest = 7, dim = 4, alpha = 0,
                           setup = "RespSparseTau1strong")$feat_tr)

simulate_causal_experiment(ntrain = 15, ntest = 7, dim = 4, alpha = .1,
                           setup = "RsparseT2weak")

simulate_causal_experiment(ntrain = 15, ntest = 7, dim = 4, alpha = .1,
                           setup = "complexTau")
