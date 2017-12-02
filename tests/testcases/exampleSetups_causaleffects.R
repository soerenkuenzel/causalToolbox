devtools::load_all()
simulate_cormat(4, 0)
cov(simulate_causal_experiment(ntrain = 1000000, ntest = 7, dim = 4, alpha = 0,
                           setup = "RespSparseTau1strong")$feat_tr)

simulate_causal_experiment(ntrain = 15, ntest = 7, dim = 4, alpha = .1,
                           setup = "RsparseT2weak")

simulate_causal_experiment(ntrain = 15, ntest = 7, dim = 4, alpha = .1,
                           setup = "complexTau")


setup_grid <- c(
  "RespSparseTau1strong",
  "RsparseT2weak",
  "complexTau",
  "Conf1",
  "rare1",
  "STMpp",
  "Ufail",
  "Usual1",
  "Wager1",
  "Wager2",
  "Wager3"
)

for(i in 1:11 ){
  simulate_causal_experiment(ntrain = 15, ntest = 7, dim = 6, alpha = .1,
                             setup = setup_grid[i])
}
