# In this file we tune X such that it
#devtools::load_all()
library(hte)

args=(commandArgs(TRUE))
print(args)
seed <- -as.numeric(args)
print(seed)

set.seed(seed)

nthread <- 1
nsamples <- 1000
dim <- 12
ntrain <- 1000
ntest <- 1000
alpha <- .1
setup <- "RsparseT2weak"

# Sample each parameter
Rand_tune <- data.frame(
  dim = dim,
  ntrain = ntrain,
  mtry_first = sample(1:dim, nsamples, replace = TRUE),
  mtry_second = sample(1:dim, nsamples, replace = TRUE),
  min_node_size_spl_first = sample(c(1, 3, 10, 30, 100), nsamples, replace = TRUE),
  min_node_size_spl_second = sample(c(1, 3, 10, 30, 100), nsamples, replace = TRUE),
  min_node_size_ave_first = sample(c(1, 3, 10, 30, 100), nsamples, replace = TRUE),
  min_node_size_ave_second = sample(c(1, 3, 10, 30, 100), nsamples, replace = TRUE),
  splitratio_first = sample(c(.05, .1, .2, .3, .5, .8), nsamples, replace = TRUE),
  splitratio_second = sample(c(.05, .1, .2, .3, .5, .8), nsamples, replace = TRUE),
  replace_first = sample(c(TRUE, FALSE), nsamples, replace = TRUE),
  replace_second = sample(c(TRUE, FALSE), nsamples, replace = TRUE),
  sample_fraction_first = sample(c(.01, .1, .3, .5, 0.632, .7, .9, 1), nsamples,
                                 replace = TRUE),
  sample_fraction_second = sample(c(.01, .1, .3, .5, 0.632, .7, .9, 1), nsamples,
                                  replace = TRUE)
)

if (!dir.exists("Tune/"))
      dir.create("Tune/")
if (!dir.exists("Tune/XLearner/"))
  dir.create("Tune/XLearner/")

# loop through rows and fill in the NA columns:
for (i in 1:nsamples) {
  print(paste("Starting with", i, "of", nsamples))
  stat.time <- Sys.time()
  # run the experiment:
  experiment <- simulate_causal_experiment(
    ntrain = ntrain,
    ntest = ntest,
    dim = dim,
    alpha = alpha,
    setup = setup
  )

  # train the X-learner:
  EMSE <-
    tryCatch({
      L <- X_RF(
        feat = experiment$feat_tr,
        tr = experiment$W_tr,
        yobs = experiment$Yobs_tr,
        predmode = "propmean",
        ntree_first = 500,
        ntree = 500,
        mtry_first = Rand_tune$mtry_first[i],
        mtry_second = Rand_tune$mtry_second[i],
        min_node_size_spl_first = Rand_tune$min_node_size_spl_first[i],
        min_node_size_spl_second = Rand_tune$min_node_size_spl_second[i],
        min_node_size_ave_first = Rand_tune$min_node_size_ave_first[i],
        min_node_size_ave_second = Rand_tune$min_node_size_ave_second[i],
        splitratio_first = Rand_tune$splitratio_first[i],
        splitratio_second = Rand_tune$splitratio_second[i],
        replace_first = Rand_tune$replace_first[i],
        replace_second = Rand_tune$replace_second[i],
        sample_fraction_first = Rand_tune$sample_fraction_first[i],
        sample_fraction_second = Rand_tune$sample_fraction_second[i],
        nthread = nthread
      )
      mean((experiment$tau_te - EstimateCate(L, experiment$feat_te)) ^ 2)
    },
    error = function(e) {
      print(e)
      NA
    })

  Rand_tune[i, setup] <- EMSE
  print(paste(
    "   Done with",
    setup,
    "Error was:",
    EMSE,
    "   and it took",
    as.numeric(Sys.time() - stat.time, units = "mins"),
    " minutes."
  ))
  write.csv(Rand_tune,
            paste0("Tune/XLearner/tuneX", setup, dim, "N", ntrain, "S", seed,
              ".csv"))
}
