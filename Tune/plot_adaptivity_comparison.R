# this file gets the best tunring parameters from our simulations
library(ggplot2)
library(dplyr)
## read in the data as list:
estimator_folders <- c("S-learner" = "Tune/SLearner/sim_data/",
                       "T-learner" = "Tune/TLearner/sim_data/",
                       "X-learner" = "Tune/XLearner/sim_data/")

tuning_settings <-
  list() # this will contain for each estimator the list of simulations
for (j in 1:length(estimator_folders)) {
  file_folder <- estimator_folders[j]
  estimator_name <- names(estimator_folders)[j]
  #file_folder <- "Tune/XLearner/sim_data/"
  file_list <- list()
  i <- 1
  for (file in dir(file_folder)) {
    print(file)
    file_list[[i]] <-
      tbl_df(read.csv(paste0(file_folder, file), as.is = TRUE))
    i <- i + 1
  }
  tuning_settings[[estimator_name]] <-
    file_list[[1]][, -c(1, ncol(file_list[[1]]))] # tuning_setting = all settings
  mergingvars <- names(tuning_settings[[estimator_name]])
  for (file in file_list) {
    file <- file[, -1]
    tuning_settings[[estimator_name]] <-
      merge(tuning_settings[[estimator_name]], file, by = mergingvars)
  }
  tuning_settings[[estimator_name]]$estimator <- estimator_name
  tuning_settings[[estimator_name]] <-
    tbl_df(tuning_settings[[estimator_name]])
}

apply(tuning_settings[[1]], 2, function(x) table(is.na(x)))

## Which settings to use?
tuning_settings[["X-learner"]] %>%
  filter(STMpp < 100, RespSparseTau1strong < 100) %>%
  ggplot(aes(x=RespSparseTau1strong, y=STMpp)) +
  geom_point()
# decided on "RsparseT2weak" "STMpp"



tuning_settings[["X-learner"]] %>%
  filter(STMpp < 100, RespSparseTau1strong < 100) %>%
  ggplot(aes(x=RespSparseTau1strong, y=STMpp)) +
  geom_point()




















