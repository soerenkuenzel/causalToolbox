library(tidyverse)
setwd('/Users/soeren/Dropbox/causalToolbox/tests/performance/selectors/sim_data')

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
performance <- unique(selector_dta %>% select(-estimator, -score))
performance$lowestMSE <- NA
performance$highestMSE <- NA
performance$chosenMSE <- NA

for (setting_i in 1:nrow(performance)) {
  print(paste(setting_i, 'of', nrow(performance)))
  setting <- performance[setting_i,]
  
  # find the lowest MSE
  true_performance <- merge(setting %>% select(-selector), truth_dta)
  dublicated <- true_performance %>% select(-MSE, -MSE_sd) %>% duplicated()
  true_performance <- true_performance[!dublicated, ]
  performance$lowestMSE[setting_i] <- min(true_performance$MSE)
  performance$highestMSE[setting_i] <- max(true_performance$MSE)
  
  # find the chosen MSE
  selector_dta_setting <- merge(setting, selector_dta)
  dublicated <- selector_dta_setting %>% select(-score) %>% duplicated()
  selector_dta_setting <- selector_dta_setting[!dublicated, ]
  chosen_estimator <- 
    selector_dta_setting$estimator[which.min(selector_dta_setting$score)]
  true_performance$MSE[true_performance$estimator == chosen_estimator]
  
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
  geom_point(position = position_jitter(width = .4, height =.01), size = .5, alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  theme(legend.position="none")
  









ggplot(data = my_datal, aes(y = Sensitivity, x = EmotionCondition, fill = EmotionCondition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Sensitivity, color = EmotionCondition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() +
  raincloud_theme


