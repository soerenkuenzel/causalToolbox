# This file reads in the data from the CATE_MSE_evaluate_pipeline and creates
# summary plots for their outcomes

library(ggplot2)
library(reshape)
library(dplyr)

Results <- data.frame()
# datafolder_CF <- "tests/performance/CATEestimators/sim_data/all/"
datafolder_CF <-
  "~/Dropbox/CATE/hte/tests/performance/CATEestimators/sim_data/2017-10-13/"
plot_save_folder <- "~/Dropbox/CATE/hte/tests/performance/CATEestimators/figs/"
for (filename in dir(datafolder_CF)) {
  # l_filename <- nchar(filename)
  # if(substr(filename, l_filename-6, l_filename-4) != "ggr") next
  newResults <- read.csv(paste0(datafolder_CF, filename)) %>% tbl_df()
  newResults
  Results <- rbind(Results, newResults)
  print(filename)
}

# setup the colors
cols <- c(
  "S_RF" = "blue",
  "T_RF" = "blue3",
  "X_RF" = "green",
  "CF" = "red",
  "CF_p" = "red2"
  # "U_half_RF" = "#00C094",
  # "X_RF_sRF" = "blue3"
)

unique(Results$estimator)
estimator_subset <- c("CF", "CF_p", "S_RF", "T_RF","X_RF ")

# Summary plots:
for (this_setup in unique(Results$setup)) {
  print(this_setup)
  plot_ncol <- length(unique(Results$dim[Results$setup == this_setup]))
  Results %>% filter(setup == this_setup) %>%
    # filter(estimator %in% estimator_subset) %>%
    group_by(ntrain, dim, setup, alpha, feat_distribution, estimator) %>%
    summarize(n = n(), MSE = mean(MSE)) %>% filter(!is.na(MSE)) %>% ungroup() %>%
    filter(dim %in% c(5, 6, 20, 100), alpha %in% c(0, 0.1, 10)) %>%
    mutate(dima = factor(
      paste0("d=", dim, ", a=", alpha),
      levels = apply(
        expand.grid("d=", as.character(unique(dim)), ", a=",
                    as.character(unique(alpha))),
        1,
        paste0,
        collapse = ''
      )
    )) %>%
    ggplot(aes(x = ntrain, y = MSE, color = estimator)) +
    geom_line() +
    geom_point(aes(shape = estimator), size = 1) +
    facet_wrap( ~ dima, scales = "free", ncol = plot_ncol) +
    scale_y_log10() +
    ggtitle(this_setup) +
    theme_minimal() +
    geom_text(aes(label = estimator))

  ggsave(
    paste0(
      plot_save_folder,
      this_setup,
      ".pdf"
    ),
    height = 8,
    width = 15
  )
}


# individual plots:
for (this_setup in unique(Results$setup)) {
  plot_ncol <- length(unique(Results$dim[Results$setup == this_setup]))
  Results %>% filter(setup == this_setup) %>%
    group_by(ntrain, dim, setup, alpha, feat_distribution, estimator) %>%
    summarize(n = n(), MSE = mean(MSE)) %>% filter(!is.na(MSE)) %>% ungroup() %>%
    filter(dim == 20, alpha == 0) %>%
    mutate(dima = factor(
      paste0("d=", dim, ", a=", alpha),
      levels = apply(
        expand.grid("d=", as.character(unique(dim)), ", a=",
                    as.character(unique(alpha))),
        1,
        paste0,
        collapse = ''
      )
    )) %>%
    ggplot(aes(x = ntrain, y = MSE, color = estimator)) +
    geom_line() +
    geom_point(aes(shape = estimator), size = 1) +
    scale_y_log10() +
    ggtitle(this_setup)
  ggsave(
    paste0(
      "tests/performance/CATEestimators/sim_figures/singleplot_",
      this_setup, "_", Sys.Date(), "_",
      "dim20a0.png"
    ),
    height =4,
    width = 8
  )
}
