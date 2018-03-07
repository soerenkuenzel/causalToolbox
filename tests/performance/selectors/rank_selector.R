# Loads in results from selector_evaluate_pipeline and ranks each estimator by
# each selector as well as the true value.

library("reshape2")

setup <- "complexTau2" # Get this as user input?

data_folder_name <- "sim_data/"
if (!dir.exists(data_folder_name)) {
  stop("No simulation data found.
        Please run selector_evaluate_pipeline.R first.")
}

filename_base <- paste0(data_folder_name, "selector_rankings_", setup)
filename <- paste0(filename_base, ".csv")
filename_truth <- paste0(filename_base, "_truth", ".csv")

if (!file.exists(filename) | !file.exists(filename_truth)) {
  stop("No simulation data found.
        Please run selector_evaluate_pipeline.R first.")
}

selector_data <- read.csv(filename)
truth_data <- read.csv(filename_truth)

# selector_data <- selector_data[!is.na(selector_data[, "score"]), ]
# truth_data <- truth_data[!is.na(truth_data[, "MSE"]), ]

selector_data_opt1 <- dcast(selector_data, ... ~ selector)
selector_data_opt2 <- dcast(selector_data, selector ~ ...)
