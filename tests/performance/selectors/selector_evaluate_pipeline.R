# File runs each selector in selector_grid on every estimator and produces a
# ranking of the methods. Each estimator is the evaluated to get another ranking
# from the MSE from the truth.

library(causalToolbox)
library(hte)
# library(grf)

library(doParallel)

# ------------------------------------------------------------------------------
## Evaluation setup configuration
args <- commandArgs(TRUE)
setup_i <- as.numeric(args[1])
nthread <- 24
registerDoParallel(nthread)

set.seed(28104)

# ------------------------------------------------------------------------------
## Evaluation setup
seed_grid <- c(1500)
dim_grid <- c(5, 20, 100)
ntrain_grid <- round(10 ^ seq(from = 2, to = 6, by = .25))
ntest <- 100000

setup_grid <- c(
  "RespSparseTau1strong",
  "RsparseT2weak",
  "complexTau",
  "complexTau2",
  "complexTau3",
  "complexTau4",
  "Conf1",
  "rare1",
  "rare2",
  "rare3",
  "STMpp",
  "STMpp2",
  "STMpp3",
  "STMpp4",
  "Ufail",
  "Usual1",
  "WA1",
  "WA2",
  "WA3",
  "WA4"
)

setup <- setup_grid[[setup_i]]

selector_grid <- list(
  "transformed" = function(Yobs, W_tr, feat, estimator) {
    gof_transformed(feat = feat,
                    yobs = Yobs,
                    tr = W_tr,
                    estimator = estimator,
                    k = 3,
                    emin = 1e-5)
  },
  "matching_ATT_replace" = function(Yobs, W_tr, feat, estimator) {
    gof_matching(feat = feat,
                 yobs = Yobs,
                 tr = W_tr,
                 estimator = estimator,
                 k = 3,
                 estimand = 'ATT',
                 replace = TRUE)
  },
  "matching_ATT" = function(Yobs, W_tr, feat, estimator) {
    gof_matching(feat = feat,
                 yobs = Yobs,
                 tr = W_tr,
                 estimator = estimator,
                 k = 3,
                 estimand = 'ATT',
                 replace = FALSE)
  },
  "matching_ATE_replace" = function(Yobs, W_tr, feat, estimator) {
    gof_matching(feat = feat,
                 yobs = Yobs,
                 tr = W_tr,
                 estimator = estimator,
                 k = 3,
                 estimand = 'ATE',
                 replace = TRUE)
  },
  "matching_ATE" = function(Yobs, W_tr, feat, estimator) {
    gof_matching(feat = feat,
                 yobs = Yobs,
                 tr = W_tr,
                 estimator = estimator,
                 k = 3,
                 estimand = 'ATE',
                 replace = FALSE)
  },
  "matching_ATC_replace" = function(Yobs, W_tr, feat, estimator) {
    gof_matching(feat = feat,
                 yobs = Yobs,
                 tr = W_tr,
                 estimator = estimator,
                 k = 3,
                 estimand = 'ATC',
                 replace = TRUE)
  },
  "matching_ATC" = function(Yobs, W_tr, feat, estimator) {
    gof_matching(feat = feat,
                 yobs = Yobs,
                 tr = W_tr,
                 estimator = estimator,
                 k = 3,
                 estimand = 'ATC',
                 replace = FALSE)
  },
  "subset" = function(Yobs, W_tr, feat, estimator) {
    gof_subset(feat = feat,
               yobs = Yobs,
               tr = W_tr,
               estimator = estimator)
  }
)

estimator_grid <- list(
  "S_RF" = function(feat, tr, yobs)
    S_RF(feat, tr, yobs, nthread = nthread),
  "T_RF" = function(feat, tr, yobs)
    T_RF(feat, tr, yobs, nthread = nthread),
  "X_RF" = function(feat, tr, yobs)
    X_RF(feat, tr, yobs, verbose = FALSE, nthread = nthread),
  "S_BART" = function(feat, tr, yobs)
    S_BART(feat, tr, yobs),
  "T_BART" = function(feat, tr, yobs)
    T_BART(feat, tr, yobs),
  "X_BART" = function(feat, tr, yobs)
    X_BART(feat, tr, yobs)
  # "CF_p" = function(feat, tr, yobs) {
  #   feat <- as.matrix(feat)
  #   colnames(feat) <- NULL
  #   propensityForest(X = feat,
  #                    W = tr,
  #                    Y = yobs,
  #                    num.trees = 500,
  #                    sample.size = nrow(feat) / 10,
  #                    nodesize = 1)
  # },
  # "CF" = function(feat, tr, yobs) {
  #   feat <- as.matrix(feat)
  #   colnames(feat) <- NULL
  #   grf::causal_forest(X = feat,
  #                      Y = yobs,
  #                      W = tr,
  #                      num.trees = 500,
  #                      num.threads = nthread)
  # }
)

CATEpredictor_grid <- list(
  "S_RF" = function(estimator, feat_te)
    hte::EstimateCate(estimator, feat_te),
  "T_RF" = function(estimator, feat_te)
    hte::EstimateCate(estimator, feat_te),
  "X_RF" = function(estimator, feat_te)
    hte::EstimateCate(estimator, feat_te),
  "S_BART" = function(estimator, feat_te)
    hte::EstimateCate(estimator, feat_te),
  "T_BART" = function(estimator, feat_te)
    hte::EstimateCate(estimator, feat_te),
  "X_BART" = function(estimator, feat_te)
    hte::EstimateCate(estimator, feat_te),
  "CF_p" = function(estimator, feat_te) {
    feat_te <- as.matrix(feat_te)
    colnames(feat_te) <- NULL
    return(predict(estimator, feat_te))
  },
  "CF" = function(estimator, feat_te)  {
    feat_te <- as.matrix(feat_te)
    colnames(feat_te) <- NULL
    return(predict(estimator, feat_te)$predictions)
  }
)

# ------------------------------------------------------------------------------
## Setting up location for saving evaluation results
data_folder_name <- "sim_data/"
if (!dir.exists(data_folder_name)) {
  dir.create(data_folder_name)
}

filename_base <- paste0(data_folder_name, "selector_data_", setup)
filename <- paste0(filename_base, ".csv")
if (file.exists(filename)) {
  already_ran <- read.csv(filename)
  #already_ran <- already_ran[!is.na(already_ran[,"MSE"]), ] #only if not NA
  already_ran <-
    already_ran[, c("seed", "alpha", "dim", "ntrain", "estimator", "selector")]
  for (i in 1:ncol(already_ran)) {
    already_ran[, i] <- as.character(already_ran[, i])
  }
}

filename_truth <- paste0(filename_base, "_truth", ".csv")
if (file.exists(filename)) {
  already_ran_truth <- read.csv(filename_truth)
  already_ran_truth <-
    already_ran_truth[, c("seed", "alpha", "dim", "ntrain", "estimator")]
}

# ------------------------------------------------------------------------------
## Loop through the setups and evaluate the estimators
for (seed in seed_grid) {
  for (dim in dim_grid) {
    print(paste("==> Starting with seed =", seed,
                "of", max(seed_grid),
                ", dim =", dim,
                "of", paste(dim_grid, collapse = ", ")))
    for (ntrain in ntrain_grid) {
      # Create training and test data
      set.seed(seed)
      dt <- simulate_causal_experiment(
        ntrain = ntrain,
        ntest = ntest,
        dim = dim,
        alpha = 0.1,
        feat_distribution = "normal",
        setup = setup,
        testseed = 293901,
        trainseed = seed
      )
      
      # Evaluate each estimator on the truth and run every selector
      for (estimator_i in 1:length(estimator_grid)) {
        estimator <- estimator_grid[[estimator_i]]
        estimator_name <- names(estimator_grid)[estimator_i]
        CATEpredictor <- CATEpredictor_grid[[estimator_name]]
        
        estimates <- tryCatch({
          L <- estimator(
            feat = dt$feat_tr,
            tr = dt$W_tr,
            yobs = dt$Yobs_tr
          )
          CATEpredictor(L, dt$feat_te)
        },
        error = function(e) {
            print(e)
            warning(paste("Something went wrong with", estimator_name,
                          "on", setup))
            return(NA)         
        })
        MSE <- mean((dt$tau_te - estimates)^2)
        MSE_sd <- sd((dt$tau_te - estimates)^2) / sqrt(length(dt$tau_te))
        
        Residuals <- data.frame(
          seed = seed,
          alpha = 0.1,
          dim = dim,
          feat_distribution = "normal",
          testseed = 293901,
          trainingseed = seed,
          ntrain = ntrain,
          estimator = estimator_name,
          setup = setup,

          MSE = MSE,
          MSE_sd = MSE_sd
        )
        
        col.names <- !file.exists(filename_truth)
        write.table(
          Residuals,
          file = filename_truth,
          append = TRUE,
          col.names = col.names,
          row.names = FALSE,
          sep = ","
        )
        
        for (selector_i in 1:length(selector_grid)) {
          selector <- selector_grid[[selector_i]]
          selector_name <- names(selector_grid)[selector_i]
          print(paste("==> Running", selector_name,
                      "on", setup))

          selector_vals <- tryCatch({
            selector(dt$Yobs_tr, dt$W_tr, dt$feat_tr, estimator)
          },
          error = function(e) {
            print(e)
            warning(paste("Something went wrong with", estimator_name,
                          "and selector", selector_name,
                          "on", setup))
            return(NA)
          })
          
          selector_data <- data.frame(
            seed = seed,
            alpha = 0.1,
            dim = dim,
            ntrain = ntrain,
            feat_distribution = "normal",
            testseed = 293901,
            trainingseed = seed,
            estimator = estimator_name,
            selector = selector_name,
            setup = setup,
            score = selector_vals[1]
          )
          
          col.names <- !file.exists(filename)
          write.table(
            selector_data,
            file = filename,
            append = TRUE,
            col.names = col.names,
            row.names = FALSE,
            sep = ","
          )
        }
        
        print(paste(
          "    Done with ntrain =",
          ntrain,
          ", estimator =",
          estimator_name
        ))
      }
    }
  }
}
