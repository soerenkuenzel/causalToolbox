# in this file we plot the confidence interval performance of different
# estimators
library(ggplot2)
library(dplyr)

files <- dir("tests/ConfidenceIntervals/data/")

CI <- data.frame()
for (file in files) {
  setup <-
    gsub("([^_]*)(_[^_]*)(_[^_]*)(_[^_]*)(_[^_.]*).csv" ,
         "\\1",
         file)
  n <-
    substr(gsub("[^_]*(_[^_]*)(_[^_]*)(_[^_]*)(_[^_.]*).csv" , "\\1", file),
           3,
           100) %>% as.numeric()
  dim <-
    substr(gsub("[^_]*(_[^_]*)(_[^_]*)(_[^_]*)(_[^_.]*).csv" , "\\2", file),
           5,
           100) %>% as.numeric()
  alpha <-
    substr(gsub("[^_]*(_[^_]*)(_[^_]*)(_[^_]*)(_[^_.]*).csv" , "\\3", file),
           7,
           100) %>% as.numeric()
  B <-
    substr(gsub("[^_]*(_[^_]*)(_[^_]*)(_[^_]*)(_[^_.]*).csv" , "\\4", file),
           3,
           100) %>% as.numeric()
  CI_i <-
    read.csv(paste0("tests/ConfidenceIntervals/data/", file),  as.is = TRUE)[, -1]
  if (is.na(CI_i[1, 1]))
    next
  CI_i <- cbind(CI_i,
                setup,
                n,
                dim,
                alpha,
                B)
  CI_i <- CI_i %>% tbl_df() %>%
    rename(tau = experiment.tau_te)
  CI <- rbind(CI, CI_i)
}
coverage <- CI %>% mutate(covered = X5. <= tau & tau  <= X95.) %>%
  summarize(sum(covered) / n()) %>% as.numeric()

set.seed(21512)
setup_loop <- unique(CI$setup)
n_loop <- unique(CI$n)
dim_loop <- unique(CI$dim)


for (setup_i in setup_loop) {
  for (n_i in n_loop) {
    for (dim_i in dim_loop) {
      CI_selected <-
        CI %>% filter(setup == setup_i, n == n_i, dim == dim_i)
      if (nrow(CI_selected) == 0)
        next

      coverage <-
        CI_selected %>% mutate(covered = X5. <= tau & tau  <= X95.) %>%
        summarize(sum(covered) / n()) %>% as.numeric()

      CI_selected_sampled <- CI_selected %>% sample_n(70)

      lower_boundary <- floor(min(CI_selected_sampled$tau))
      upper_boundary <- ceiling(max(CI_selected_sampled$tau))

      CI_selected_sampled %>%
        ggplot() +
        geom_point(aes(x = tau, y = pred)) +
        geom_errorbar(aes(
          x = tau,
          ymax = X95.,
          ymin = X5.
        )) +
        geom_abline(slope = 1) +
        coord_cartesian(
          xlim = c(lower_boundary, upper_boundary),
          ylim = c(lower_boundary, upper_boundary)
        ) +
        theme_bw() +
        ylab("predicted CATE") +
        xlab("true CATE") +
        ggtitle(paste0(setup, ", dim  =", dim_i, ", n =", n_i, ", alpha = .1",  ", coverage =", coverage))

      ggsave(
        paste0(
          "tests/ConfidenceIntervals/plots/X_",
          setup_i,
          "_dim",
          dim_i,
          "_n",
          n_i,
          ".pdf"
        )
      )
    }
  }
}
