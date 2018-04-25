library(tidyverse)
#setwd('/Users/soeren/Dropbox/causalToolbox/tests/performance/selectors/sim_data_3/')
setwd('/Users/lingxie/Desktop/urap/causalToolbox/tests/performance/selectors/sim_data_3/')
# -- Read in all the data ------------------------------------------------------
available_files <- 
  data.frame(file = dir(), 
             setting = gsub("([^_]*)_([^_]*)_([^_.]*).*", "\\3", dir()),
             truth = grepl('truth', dir()), 
             stringsAsFactors = FALSE)
available_files <- available_files[grepl('csv', available_files$file), ]
all_settings <- unique(available_files$setting)

selector_dta <- data.frame()
truth_dta <- data.frame()
for (i in 1:nrow(available_files)) {
  if (available_files$truth[i]) {
    truth_dta <- rbind(truth_dta, read.csv(available_files$file[i]))
  } else {
    selector_dta <- rbind(selector_dta, read.csv(available_files$file[i]))
  }
}
truth_dta <- tbl_df(truth_dta)
selector_dta <- tbl_df(selector_dta)

# -- Compute for each setting the performance of the selected method -----------
performance <- unique(selector_dta %>% select(-estimator, -score, -sd))
performance$lowestMSE <- NA
performance$highestMSE <- NA
performance$chosenMSE <- NA

for (setting_i in 1:nrow(performance)) {
  if (setting_i%%50 == 0) print(paste(setting_i, 'of', nrow(performance)))
  setting <- performance[setting_i,]
  
  # find the lowest MSE
  true_performance <- merge(setting %>% select(-selector), truth_dta)
  dublicated <- true_performance %>% select(-MSE, -MSE_sd) %>% duplicated()
  if(!all(!dublicated)) stop("There are dublicates")
  true_performance <- true_performance[!dublicated, ]
  performance$lowestMSE[setting_i] <- min(true_performance$MSE)
  performance$highestMSE[setting_i] <- max(true_performance$MSE)
  
  # find the chosen MSE
  selector_dta_setting <- merge(setting, selector_dta)
  dublicated <- selector_dta_setting %>% select(-score) %>% duplicated()
  if(!all(!dublicated)) stop("There are dublicates")
  selector_dta_setting <- selector_dta_setting[!dublicated, ]
  chosen_estimator <- 
    selector_dta_setting$estimator[which.min(selector_dta_setting$score)]
  # true_performance$MSE[true_performance$estimator == chosen_estimator]
  
  if(length(chosen_estimator) == 0) next
  performance$chosenMSE[setting_i] <- 
    true_performance$MSE[true_performance$estimator == chosen_estimator]
}

# -- Plot 1 - Setting vs best performance --------------------------------------
apply(performance, 2, function(x)length(unique(x)))
# dim, ntrain, and setup identify everything.

performance %>% mutate(setting = paste0(setup, '_', dim, '_', ntrain)) %>%
  select(setting, selector, lowestMSE, highestMSE, chosenMSE) %>%  
  filter(selector %in% c('transformed', 'matching_ATT', 'subset')) %>%
  ggplot(aes(x = setting, 
             y = (chosenMSE - lowestMSE) / (highestMSE - lowestMSE), 
             color = selector)) +
  geom_point() 

# -- Plot 2 - Histogram --------------------------------------------------------
apply(performance, 2, function(x)length(unique(x)))
# dim, ntrain, and setup identify everything.

performance %>% mutate(setting = paste0(setup, '_', dim, '_', ntrain)) %>%
  select(setting, selector, lowestMSE, highestMSE, chosenMSE) %>%  
  # filter(selector %in% c('transformed', 'matching_ATT', 'subset')) %>%
  ggplot(aes(x = selector, 
             y = (chosenMSE - lowestMSE) / (highestMSE - lowestMSE), 
             color = selector)) +
  geom_violin() + 
  geom_point(position = position_jitter(width = .4, height =.01), size = .5,
             alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  theme(legend.position = "none")
  
ggsave('../ChosenMSE_violin_plot_3.pdf', height = 8, width = 10)


performance_nonconstant_e <- performance[performance$setup == c('WA1','Conf1'),  ]
apply(performance_nonconstant_e, 2, function(x)length(unique(x)))

performance_nonconstant_e %>% mutate(setting = paste0(setup, '_', dim, '_', ntrain)) %>%
  select(setting, selector, lowestMSE, highestMSE, chosenMSE) %>%  
  # filter(selector %in% c('transformed', 'matching_ATT', 'subset')) %>%
  ggplot(aes(x = selector, 
             y = (chosenMSE - lowestMSE) / (highestMSE - lowestMSE), 
             color = selector)) +
  geom_violin() + 
  geom_point(position = position_jitter(width = .4, height =.01), size = .5,
             alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  theme(legend.position = "none")
ggsave('../nonconstant_e_violin_plot.pdf', height = 8, width = 10)

performance_constant_e <- performance[performance$setup != c('WA1','Conf1'),  ]
apply(performance_constant_e, 2, function(x)length(unique(x)))

performance_constant_e %>% mutate(setting = paste0(setup, '_', dim, '_', ntrain)) %>%
  select(setting, selector, lowestMSE, highestMSE, chosenMSE) %>%  
  # filter(selector %in% c('transformed', 'matching_ATT', 'subset')) %>%
  ggplot(aes(x = selector, 
             y = (chosenMSE - lowestMSE) / (highestMSE - lowestMSE), 
             color = selector)) +
  geom_violin() + 
  geom_point(position = position_jitter(width = .4, height =.01), size = .5,
             alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  theme(legend.position = "none")
ggsave('../constant_e_violin_plot.pdf', height = 8, width = 10)

performance_WA1 <- performance[performance$setup == c('WA1'),  ]
apply(performance_WA1, 2, function(x)length(unique(x)))

performance_WA1 %>% mutate(setting = paste0(setup, '_', dim, '_', ntrain)) %>%
  select(setting, selector, lowestMSE, highestMSE, chosenMSE) %>%  
  # filter(selector %in% c('transformed', 'matching_ATT', 'subset')) %>%
  ggplot(aes(x = selector, 
             y = (chosenMSE - lowestMSE) / (highestMSE - lowestMSE), 
             color = selector)) +
  geom_violin() + 
  geom_point(position = position_jitter(width = .4, height =.01), size = .5,
             alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  theme(legend.position = "none")
ggsave('../WA1_violin_plot.pdf', height = 8, width = 10)

performance_Conf1 <- performance[performance$setup == c('Conf1'),  ]
apply(performance_Conf1, 2, function(x)length(unique(x)))

performance_Conf1 %>% mutate(setting = paste0(setup, '_', dim, '_', ntrain)) %>%
  select(setting, selector, lowestMSE, highestMSE, chosenMSE) %>%  
  # filter(selector %in% c('transformed', 'matching_ATT', 'subset')) %>%
  ggplot(aes(x = selector, 
             y = (chosenMSE - lowestMSE) / (highestMSE - lowestMSE), 
             color = selector)) +
  geom_violin() + 
  geom_point(position = position_jitter(width = .4, height =.01), size = .5,
             alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  theme(legend.position = "none")
ggsave('../Conf1_violin_plot.pdf', height = 8, width = 10)

performance_0.5 <- performance[performance$setup == c("RespSparseTau1strong",
                                                      "RsparseT2weak", 
                                                      "STMpp", "STMpp2", 
                                                      "STMpp3", "STMpp4", 
                                                      "Ufail", "Usual1", 
                                                      "WA2", "WA3", "WA4") ,  ]
apply(performance_0.5, 2, function(x)length(unique(x)))

performance_0.5 %>% mutate(setting = paste0(setup, '_', dim, '_', ntrain)) %>%
  select(setting, selector, lowestMSE, highestMSE, chosenMSE) %>%  
  # filter(selector %in% c('transformed', 'matching_ATT', 'subset')) %>%
  ggplot(aes(x = selector, 
             y = (chosenMSE - lowestMSE) / (highestMSE - lowestMSE), 
             color = selector)) +
  geom_violin() + 
  geom_point(position = position_jitter(width = .4, height =.01), size = .5,
             alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  theme(legend.position = "none")

ggsave('../e0.5_violin_plot.pdf', height = 8, width = 10)

performance_0.01 <- performance[performance$setup == c("rare1","rare2", 
                                                      "rare3"),  ]
apply(performance_0.01, 2, function(x)length(unique(x)))

performance_0.01 %>% mutate(setting = paste0(setup, '_', dim, '_', ntrain)) %>%
  select(setting, selector, lowestMSE, highestMSE, chosenMSE) %>%  
  # filter(selector %in% c('transformed', 'matching_ATT', 'subset')) %>%
  ggplot(aes(x = selector, 
             y = (chosenMSE - lowestMSE) / (highestMSE - lowestMSE), 
             color = selector)) +
  geom_violin() + 
  geom_point(position = position_jitter(width = .4, height =.01), size = .5,
             alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  theme(legend.position = "none")
ggsave('../e0.01_violin_plot.pdf', height = 8, width = 10)