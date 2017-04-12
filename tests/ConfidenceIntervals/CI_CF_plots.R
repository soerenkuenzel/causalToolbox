library(ggplot2)
library(dplyr)


#################################
### Read in the data ############
#################################
files <- dir("tests/ConfidenceIntervals/data/")

CI <- data.frame()
for (file in files) {
  if (substr(file, 1, 12) != "causalForest")
    next
  file_setup <- substr(file, 15, 100)
  setup <-
    gsub("([^_]*)(_[^_]*)(_[^_]*)(_[^_]*)(_[^_.]*).csv" ,
         "\\1",
         file_setup)
  n <-
    substr(gsub("[^_]*(_[^_]*)(_[^_]*)(_[^_]*)(_[^_.]*).csv" , "\\1", file_setup),
           3,
           100) %>% as.numeric()
  dim <-
    substr(gsub("[^_]*(_[^_]*)(_[^_]*)(_[^_]*)(_[^_.]*).csv" , "\\2", file_setup),
           5,
           100) %>% as.numeric()
  alpha <-
    substr(gsub("[^_]*(_[^_]*)(_[^_]*)(_[^_]*)(_[^_.]*).csv" , "\\3", file_setup),
           7,
           100) %>% as.numeric()
  B <-
    substr(gsub("[^_]*(_[^_]*)(_[^_]*)(_[^_]*)(_[^_.]*).csv" , "\\4", file_setup),
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
table(CI$var.hat)


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

      CI_selected_sampled <- CI_selected %>% sample_n(70)

      lower_boundary <- floor(min(CI_selected_sampled$tau))
      upper_boundary <- ceiling(max(CI_selected_sampled$tau))

      CI_selected_sampled %>%
        ggplot() +
        geom_point(aes(x = tau, y = predictions)) +
        geom_abline(slope = 1) +
        coord_cartesian(
          xlim = c(lower_boundary, upper_boundary),
          ylim = c(lower_boundary, upper_boundary)
        ) +
        theme_bw() +
        ylab("estimated CATE") +
        xlab("true CATE") +
        ggtitle(paste0(
          "dim  =",
          dim_i,
          ", n =",
          n_i,
          ", alpha = .1"
        ))

      ggsave(
        paste0(
          "tests/ConfidenceIntervals/plots/CF_",
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


