# this file gets the best tunring parameters from our simulations
library(ggplot2)
library(dplyr)
## read in the data as list:
estimator_folders <- c("S-learner" = "Tune/SLearner/sim_data/",
                       "T-learner" = "Tune/TLearner/sim_data/",
                       "X-learner" = "Tune/XLearner/sim_data/")

tuning_settings <- list()
# this will contain for each estimator the list of simulations
for (j in 1:length(estimator_folders)) {
  file_folder <- estimator_folders[j]
  estimator_name <- names(estimator_folders)[j]
  #file_folder <- "Tune/XLearner/sim_data/"
  file_list <- list()
  i <- 1
  for (file in dir(file_folder)) {
    print(file)
    file_list[[i]] <-
      read.csv(paste0(file_folder, file), as.is = TRUE)
    i <- i + 1
  }
  tuning_settings[[estimator_name]] <-
    file_list[[1]][, -c(1, ncol(file_list[[1]]))] # tuning_setting = all settings
  mergingvars <- names(tuning_settings[[estimator_name]])
  for (file in file_list) {
    file <- file[, -1]
    tuning_settings[[estimator_name]] <-
      merge(tuning_settings[[estimator_name]],
            file,
            by = mergingvars)
  }
  tuning_settings[[estimator_name]]$estimator <- estimator_name
  tuning_settings[[estimator_name]] <-
    tbl_df(tuning_settings[[estimator_name]])
  dots <- lapply(mergingvars, as.symbol)
  tuning_settings[[estimator_name]] <-
    tuning_settings[[estimator_name]] %>%
    group_by_(.dots = dots) %>%
    summarize(
      complexTau = mean(complexTau),
      Conf1 = mean(Conf1),
      rare1 = mean(rare1),
      RespSparseTau1strong = mean(RespSparseTau1strong),
      RsparseT2weak = mean(RsparseT2weak),
      STMpp = mean(STMpp),
      Ufail = mean(Ufail),
      Usual1 = mean(Usual1),
      Wager1 = mean(Wager1),
      Wager2 = mean(Wager2),
      Wager3 = mean(Wager3)
    ) %>%
    ungroup()
}
# tuning_settings

## Which settings to use?
tuning_settings[["X-learner"]] %>%
  filter(STMpp < 100, RespSparseTau1strong < 100) %>%
  ggplot(aes(x = RespSparseTau1strong, y = STMpp)) +
  geom_point()
# decided on "RsparseT2weak" "STMpp"

tuningsettings_outcomes <- rbind(
  cbind(tuning_settings[["S-learner"]][, 9:19], estimator = "S-learner"),
  cbind(tuning_settings[["T-learner"]][, 9:19], estimator = "T-learner"),
  cbind(tuning_settings[["X-learner"]][, 15:25], estimator = "X-learner")
)


# Searching for good settings:
colnames(tuningsettings_outcomes)

tuningsettings_outcomes %>%
  filter(Wager1 < .05, RespSparseTau1strong < 500) %>%
  ggplot(aes(x = RespSparseTau1strong, y = Wager1, color = estimator)) +
  geom_point(alpha = 1)

tuningsettings_outcomes %>%
  filter(Wager1 < .05, RsparseT2weak < 500) %>%
  ggplot(aes(x = RsparseT2weak, y = Wager1, color = estimator)) +
  geom_point(alpha = 1)

tuningsettings_outcomes %>%
  filter(Wager1 < .05, Wager3 < .1) %>%
  ggplot(aes(x = RsparseT2weak, y = Usual1, color = estimator)) +
  geom_point(alpha = .5)




#### creation of the final plot:
tuningsettings_outcomes %>% mutate(Estimator = estimator) %>%
  filter(Wager1 < .04) %>%
  ggplot(aes(x = complexTau, y = Wager1, color = Estimator)) +
  geom_point(alpha = .5) +
  scale_color_manual(values = c("green", "chocolate", "blue")) +
  theme_minimal() +
  xlab("MSE for Complex Treatment Effect and no Confounding") +
  ylab("MSE for Simple Treatment Effect with Confounding")

ggsave(
  paste0("Tune/adaptivityfigue.pdf"),
  height = 5,
  width = 6
)
