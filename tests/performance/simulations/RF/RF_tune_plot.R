library(ggplot2)
library(reshape)
library(dplyr)

################# Multiplot #################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
################# Multiplot #################

# read in the data
datafolder <- "tests/performance/simulations/RF/sim_data/"
plotfolder <- "tests/performance/simulations/RF/sim_plot/"

top_n <- 100

for (filename in dir(datafolder)) {
  setup_data <- data.frame()
  if (grepl("tuning_", filename)) {
    newResults <- read.csv(paste0(datafolder, filename))
    newResults$replace <- as.numeric(newResults$replace)
    newResults$mtry <- newResults$mtry / newResults$dim
    newResults <- newResults[with(newResults, order(newResults[10])), ]
    newResults = newResults[1:top_n, ]
    setup_data <- rbind(setup_data, newResults)

    # setup_data <- na.omit(setup_data[, -1])

    # Split Ratio
    p1 <- ggplot(setup_data, aes(x=splitratio)) +
      geom_histogram(aes(y=..density..), binwidth=.1,
                     colour="black", fill="white") +
      # geom_density(alpha=.2, fill="#FF6666") +
      ggtitle("Split Ratio")

    # mtry
    p2 <- ggplot(setup_data, aes(x=mtry)) +
      geom_histogram(aes(y=..density..), binwidth=.1,
                     colour="black", fill="white") +
      # geom_density(alpha=.2, fill="#FF6666") +
      ggtitle("mtry Ratio")


    # min_node_size_spl
    p3 <- ggplot(setup_data, aes(x=min_node_size_spl)) +
      geom_histogram(aes(y=..density..), binwidth=1,
                     colour="black", fill="white") +
      # geom_density(alpha=.2, fill="#FF6666") +
      scale_x_log10() +
      ggtitle("Minimum Splitting Node Size")

    # min_node_size_avg
    p4 <- ggplot(setup_data, aes(x=min_node_size_ave)) +
      geom_histogram(aes(y=..density..), binwidth=1,
                     colour="black", fill="white") +
      # geom_density(alpha=.2, fill="#FF6666") +
      scale_x_log10() +
      ggtitle("Minimum Averaging Node Size")

    # replace
    p5 <- ggplot(setup_data, aes(x=replace)) +
      geom_histogram(aes(y=..density..), binwidth=1,
                     colour="black", fill="white") +
      # geom_density(alpha=.2, fill="#FF6666") +
      ggtitle("Replacement Indicator")

    # sample_fraction
    p6 <- ggplot(setup_data, aes(x=sample_fraction)) +
      geom_histogram(aes(y=..density..), binwidth=.1,
                     colour="black", fill="white") +
      # geom_density(alpha=.2, fill="#FF6666") +
      ggtitle("Sample Fraction")

    output_file_name = gsub(".csv", ".pdf", filename)
    pdf(paste0(plotfolder, output_file_name),
        height = 11, width = 8.5, paper = "letter")
    multiplot(p1, p2, p3, p4, p5, p6, cols=2)
    dev.off()
  }
}
