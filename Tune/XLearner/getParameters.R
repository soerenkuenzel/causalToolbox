# this file gets the best tunring parameters from our simulations

# read in the data:
files <- dir("Tune/XLearner/")
aggregated_files <- data.frame()
for(file in files){
  print(file)
  current_file <- read.csv(paste0("Tune/XLearner/", file), as.is = TRUE)
  aggregated_files <- rbind(aggregated_files, current_file)
}
library(dplyr)
dim(aggregated_files)
aggregated_files <- aggregated_files[,-1]
aggregated_files <- aggregated_files %>% mutate(RsparseT2weak = as.numeric(RsparseT2weak)) %>%
  filter(!is.na(RsparseT2weak))

# find the good settings:
hist(aggregated_files$RsparseT2weak)
good_settings_indi <- aggregated_files$RsparseT2weak < 3
table(good_settings_indi)

good_settings <- aggregated_files %>% filter(good_settings_indi)


good_settings %>% arrange(RsparseT2weak)

