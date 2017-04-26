# in this file we plot the confidence interval performance of different
# estimators
library(ggplot2)
library(dplyr)


#################################
### Read in the data ############
#################################
files <- dir("tests/ConfidenceIntervals/data/")

CI <- data.frame()
for (file in files) {
  if (substr(file, 1, 12) == "causalForest")
    next
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


########################################
###### Single plots ####################
########################################

for (setup_i in setup_loop) {
  for (n_i in n_loop) {
    for (dim_i in dim_loop) {
      CI_selected <-
        CI %>% filter(setup == setup_i, n == n_i, dim == dim_i)
      if (nrow(CI_selected) == 0)
        next

      coverage <-
        CI_selected %>% mutate(covered = X5. <= tau &
                                 tau  <= X95.) %>%
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
        ylab("estimated CATE") +
        xlab("true CATE") +
        ggtitle(paste0(
          "dim  = ",
          dim_i,
          ", n = ",
          n_i,
          ", coverage = ",
          coverage
        ))

      ggsave(
        paste0(
          "tests/ConfidenceIntervals/plots/X_",
          setup_i,
          "_dim",
          dim_i,
          "_n",
          n_i,
          ".pdf"
        ),
        width = 11,
        height = 7
      )
      print("done with one image")
    }
  }
}

##############################################
######### Combined plots #####################
##############################################
setup_loop <- unique(CI$setup)
n_loop <- unique(CI$n)
dim_loop <- unique(CI$dim)

CI_selected_sampled <- CI %>% filter(setup == "RsparseT2weak", n %in% c(100, 1000, 10000))
lower_boundary <- floor(min(CI_selected_sampled$tau))
upper_boundary <- ceiling(max(CI_selected_sampled$tau))

set.seed(4123)
CI %>% filter(setup == "RsparseT2weak", n %in% c(100, 1000, 10000), dim %in% c(20, 100)) %>%
  mutate(n = paste("n =", n), dim = paste("dim =", dim)) %>%
  mutate(dim = factor(dim,
                      levels = c("dim = 20", "dim = 50", "dim = 100" ))) %>%
  group_by(dim, n) %>% sample_n(80) %>%
  ggplot() +
  geom_point(aes(x = tau, y = pred)) +
  geom_errorbar(aes(
    x = tau,
    ymax = X95.,
    ymin = X5.
  )) +
  geom_abline(slope = 1) +
  coord_cartesian(
    xlim = c(-18, 18),
    ylim = c(-18, 18)
  ) +
  theme_bw() +
  ylab("estimated CATE") +
  xlab("true CATE") +
  facet_grid(dim~n) +
  ggtitle(expression(paste(Y == W, "(", 4 * X[3] - 2 * X[1], ")",  + 3 * X[1] + 5* X[2]  + epsilon,
                           "     ", W , " ~ Bern(.5)")))

ggsave(
  file = "tests/ConfidenceIntervals/plots/X__Summary_RsparseT2weak.pdf",
  width = 11,
  height = 7
)




