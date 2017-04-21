library(ggplot2)
library(dplyr)
library(plyr)

dir_name <- "tests/performance/simulations/RF/sim_data/"
plot_dir_name <- "tests/performance/simulations/RF/sim_plot/"

file_name <- c(
  "honesty_randomNoise_2017-04-19.csv",
  "honesty_HighSignalToNoiseLinearModel_2017-04-19.csv",
  "honesty_LowSignalToNoiseLinearModel_2017-04-19.csv"
)

setup_loop <- 1:length(file_name)
field_loop <- c("MSE")#, "MSE_sd")

for (setup_i in setup_loop) {
  for (field in field_loop) {
    sim_data <-
      read.csv(paste0(dir_name,
                      file_name[setup_i]),
               as.is = TRUE) %>%
      dplyr::tbl_df() %>%
      dplyr::group_by(ntrain, dim, alpha, splitratio) %>%
      dplyr::filter(!is.na(MSE)) %>%
      dplyr::summarize(MSE_mean = mean(MSE),
                       MSE_sd = sd(MSE) / sqrt(n()))

    # sim_selected <- data.frame(sim_data)
    #
    # if (nrow(sim_selected) == 0)
    #   next
    #
    # sim_selected <-
    #   ddply(sim_selected, .(ntrain, dim, alpha, splitratio),
    #         function(x)
    #           c(data = mean(x[, field])))
    # sim_selected$splitratio <- as.factor(sim_selected$splitratio)
    #
    # lower_boundary <- floor(min(sim_selected$data))
    # upper_boundary <- ceiling(max(sim_selected$data))

    title["ran"] <-
      "MSE in a Setting when the Regression Function is 0"
    title["Low"] <-
      "MSE in a Setting with a Low Signal To Noise Ratio"
    title["Hig"] <-
      "MSE in a Setting with a High Signal To Noise Ratio"

    sim_data %>% tbl_df() %>%
      filter(dim %in% c(5, 20), alpha %in% c(0, 5)) %>%
      mutate(dimalpha = factor(
        paste0("d = ", dim, ", a = ", alpha),
        levels = c("d = 5, a = 0", "d = 5, a = 5",
                   "d = 20, a = 0", "d = 20, a = 5")
      ),
      splitratio = factor(splitratio)) %>%
      ggplot(aes(x = ntrain,
                 y = MSE_mean,
                 # shape = splitratio,
                 color = splitratio)) +
      geom_line() +
      geom_errorbar(aes(
        ymin=MSE_mean - 1.96 * MSE_sd, ymax=MSE_mean + 1.96 * MSE_sd
      )) +
      scale_x_log10() +
      facet_wrap( ~ dimalpha, scales = "free_y") +
      ylab(field) +
      xlab("Number of Observations") +
      theme_minimal() +
      scale_color_manual(
        values = 1:5,
        name = "Fraction of\nSplitting Samples",
        breaks = c(0.2, 0.4, 0.6, 0.8, 1),
        labels = c("20%", "40%", "60%", "80%", "adaptive")
      ) +
      # scale_shape_manual(
      #   values = 1:5,
      #   name = "Fraction of\nSplitting Samples",
      #   breaks = c(0.2, 0.4, 0.6, 0.8, 1),
      #   labels = c("20%", "40%", "60%", "80%", "adaptive")
      # ) +
      ggtitle(title[substr(file_name[setup_i], 9, 11)])

    ggsave(
      paste0(
        plot_dir_name,
        "honesty_",
        field,
        "_on_",
        strsplit(file_name[setup_i], "_")[[1]][2],
        "_",
        Sys.Date(),
        ".pdf"
      ),
      width = 11,
      height = 7
    )
    print(paste("done with", field, "on", strsplit(file_name[setup_i], "_")[[1]][2]))
  }
}
