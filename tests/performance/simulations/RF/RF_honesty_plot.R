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

      sim_data <- read.csv(paste0(dir_name,
                                  file_name[setup_i]),
                                  as.is = TRUE)

      sim_selected <- data.frame(sim_data)

      if (nrow(sim_selected) == 0)
        next

      sim_selected <-
        ddply(sim_selected, .(ntrain, dim, alpha, splitratio),
              function(x) c(data = mean(x[,field])))
      sim_selected$splitratio <- as.factor(sim_selected$splitratio)

      lower_boundary <- floor(min(sim_selected$data))
      upper_boundary <- ceiling(max(sim_selected$data))

      m <- ggplot(
        sim_selected,
        aes(x = ntrain, y = data, shape=splitratio, colour=splitratio, group=splitratio)) +
        geom_line() +
        geom_point() +
        scale_x_log10() +
        facet_grid(dim ~ alpha, labeller = label_both, scales = "free") +
        ylab(field) +
        xlab("Number of observations")+
        ggtitle(paste(field, "on", strsplit(file_name[setup_i], "_")[[1]][2])) +
        theme_bw()

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

