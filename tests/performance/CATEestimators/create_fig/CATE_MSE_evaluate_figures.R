# This file reads in the data from the CATE_MSE_evaluate_pipeline and creates
# summary plots for their outcomes

library(ggplot2)
library(reshape)
library(dplyr)

# read in the data
datafolder <- "tests/performance/CATEestimators/sim_data/all/"
Results <- data.frame()
for (filename in dir(datafolder)) {
  l_filename <- nchar(filename)
  if(substr(filename, l_filename - 5, l_filename- 4) != "26") next
  newResults <- read.csv(paste0(datafolder, filename))
  Results <- rbind(Results, newResults)
  print(filename)
}
Results <- tbl_df(Results[, -14])


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
estimator_subset <- c("X_RF0.1.0.00.1.0.0", "X_RF0.1.0.3", "X_RF0.1.0.0", "X_RF_autotune0.1.0.3")

# Summary plots:
for (this_setup in unique(Results$setup)) {
  plot_ncol <- length(unique(Results$dim[Results$setup == this_setup]))
  Results %>% filter(setup == this_setup) %>%
    # filter(estimator %in% estimator_subset) %>%
    group_by(ntrain, dim, setup, alpha, feat_distribution, estimator) %>%
    summarize(n = n(), MSE = mean(MSE)) %>% filter(!is.na(MSE)) %>% ungroup() %>%
    filter(dim %in% c(5, 20, 100), alpha %in% c(0, 0.1, 10)) %>%
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


  # scale_shape_manual(values = LETTERS[1:26]) +
  # coord_cartesian(xlim = NULL, ylim = NULL) +
  # scale_y_sqrt() +
  # scale_x_log10() +
  # facet_grid(setup ~ dim, scales = "free") +
  # theme(legend.position = "bottom") +
  # ylab("Estimated EMSE") +
  #   ggtitle(this_setup)
  #

  ggsave(
    paste0(
      "tests/performance/CATEestimators/sim_figures/",
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




# plotSetup1 <-
#   function(thissetup,
#            DS = Results,
#            otherEstimators = "",
#            upperl = .5,
#            underl = 0) {
#     # this function plots the chosen setup
#     DS %>% filter(setup == thissetup,
#                   Estimator %in% c("S_RF", "X_RF", "T_RF", "X_RF_sRF", "U_RF")) %>%
#       mutate(dim = factor(paste("d = ", dim),
#                           levels = paste("d = ", unique(dim)))) %>%
#       ggplot(aes(
#         x = ntrain,
#         y = MSE,
#         color = Estimator,
#         linetype = Estimator
#       )) +
#       theme_bw(base_size = 10, base_family = "Helvetica") +
#       geom_line() +
#       geom_point(aes(shape = Estimator), size = 1.5) +
#       geom_errorbar(aes(ymin = MSE - 2 * MSE.sd, ymax = MSE + 2 * MSE.sd), width =
#                       100) +
#       #  scale_shape_manual(values=LETTERS[1:26])  +
#       coord_cartesian(ylim = c(underl, upperl)) +
#       theme(legend.position = "bottom") +
#       ylab("EMSE") +
#       scale_y_sqrt() +
#       facet_wrap( ~ dim, scales = "free_x") +
#       xlab("Number of trainings samples")
#   }
#
# plotSetup2 <-
#   function(thissetup,
#            DS = Results,
#            otherEstimators = "",
#            upperl = .5,
#            underl = 0) {
#     # this function plots the chosen setup
#     DS %>% filter(setup == thissetup,
#                   Estimator %in% c("S_RF", "X_RF", "T_RF", "X_RF_sRF", "U_RF")) %>%
#       mutate(dim = factor(paste("d = ", dim),
#                           levels = paste("d = ", unique(dim)))) %>%
#       ggplot(aes(x = ntrain, y = MSE, color = Estimator)) +
#       theme_bw(base_size = 10, base_family = "Helvetica") +
#       geom_line() +
#       geom_point(aes(shape = Estimator), size = 1.5) +
#       #  scale_shape_manual(values=LETTERS[1:26])  +
#       coord_cartesian(ylim = c(underl, upperl)) +
#       theme(legend.position = "bottom") +
#       ylab("EMSE") +
#       facet_wrap( ~ dim, scales = "free_x") +
#       xlab("Number of trainings samples")
#   }
#
# # Simulaiton 1: different outcomes
# plotSetup2("TTMpp", otherEstimators = "")
# ggsave(
#   paste0("~/Dropbox/CATE/Xmodel/figures/TTMpp.pdf"),
#   height = 4,
#   width = 5.9
# )
#
# # Simulaiton 2: condfounding but no treatment effect
# plotSetup2("Wager1", Results %>% filter(dim %in% c(5, 10, 30)), otherEstimators = "")
# ggsave(
#   paste0("~/Dropbox/CATE/Xmodel/figures/Wager1.pdf"),
#   height = 4,
#   width = 5.9
# )
#
# # Simulaiton 3: one class is very rare
# plotSetup1(
#   "rare1",
#   otherEstimators = "",
#   underl = 0.05,
#   upperl = 20
# )
# ggsave(
#   paste0("~/Dropbox/CATE/Xmodel/figures/rare1.pdf"),
#   height = 3,
#   width = 5.9
# )
#
#
# # Simulaiton 4: Sparsity
# Results %>% filter(setup == "Usual4",
#                    Estimator %in% c("S_RF", "X_RF", "T_RF", "X_RF_sRF", "U_RF")) %>%
#   mutate(dim = factor(paste("d = ", dim),
#                       levels = paste("d = ", unique(dim)))) %>%
#   ggplot(aes(
#     x = ntrain,
#     y = MSE,
#     color = Estimator,
#     linetype = Estimator
#   )) +
#   theme_bw(base_size = 10, base_family = "Helvetica") +
#   geom_line() +
#   geom_point(aes(shape = Estimator), size = 1.5) +
#   geom_errorbar(aes(ymin = MSE - 2 * MSE.sd, ymax = MSE + 2 * MSE.sd), width =
#                   100) +
#   #  scale_shape_manual(values=LETTERS[1:26])  +
#   coord_cartesian(ylim = c(0, 3)) +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = cols) +
#   ylab("EMSE") +
#   facet_wrap( ~ dim, scales = "free_x") +
#   xlab("Number of trainings samples")
# # + scale_y_sqrt()
# ggsave(
#   paste0("~/Dropbox/CATE/Xmodel/figures/Usual4.pdf"),
#   height = 2.38,
#   width = 5.9
# )
#
#
#
# ################################################################################
# # all images in one plot
#
# # # first try:
# # Results %>% filter(setup %in% c("TTMpp", "STMpp", "Conf2", "Usual4"),
# #                    Estimator %in% c("S_RF", "X_RF", "X_RF_sRF", "T_RF", "U_RF")) %>%
# #   mutate(setup = as.character(setup),
# #          setup = ifelse(setup == "TTMpp", "Simulation 1", setup),
# #          setup = ifelse(setup == "STMpp",  "Simulation 2", setup),
# #          setup = ifelse(setup == "Conf2",  "Simulation 3", setup),
# #          setup = ifelse(setup == "Usual4",  "Simulation 4", setup)) %>%
# #   mutate(dim = ifelse(dim == 30, 100, dim)) %>%
# #   filter(dim %in% c(5, 20, 100)) %>%
# #   mutate(dim = factor(paste("d = ", dim),
# #                       levels = paste("d = ", unique(dim)))) %>%
# #   ggplot( aes(x = ntrain, y = MSE, color = Estimator)) +
# #   theme_bw(base_size = 10, base_family = "Helvetica") +
# #   geom_line() +
# #   geom_point(aes(shape = Estimator), size = 1.5) +
# #   #  scale_shape_manual(values=LETTERS[1:26])  +
# #   # coord_cartesian(ylim = c(0, .4), xlim = c(0,2000)) +
# #   theme(legend.position="bottom") +
# #   scale_y_sqrt() +
# #   ylab("EMSE") +
# #   facet_grid(setup~dim, scales = "free", space= "free_x", shrink = FALSE) +
# #   xlab("Number of training samples")
# #
# #
# #
# # # second try:
# # Results %>% filter(setup %in% c("TTMpp", "STMpp", "Conf2", "Usual4", "rare1"),
# #                    Estimator %in% c("S_RF", "X_RF", "X_RF_sRF", "T_RF", "U_RF")) %>%
# #   mutate(setup = as.character(setup),
# #          setup = ifelse(setup == "TTMpp", "Simulation 1", setup),
# #          setup = ifelse(setup == "STMpp",  "Simulation 2", setup),
# #          setup = ifelse(setup == "Conf2",  "Simulation 5", setup),
# #          setup = ifelse(setup == "rare1",  "Simulation 4", setup),
# #          setup = ifelse(setup == "Usual4",  "Simulation 3", setup)) %>%
# #   mutate(dim = ifelse(dim == 30, 100, dim)) %>%
# #   mutate(dimSet = paste0(setup, ", ", dim)) %>%
# #   filter(dim %in% c(5, 20, 100)) %>%
# #   mutate(dim = factor(paste("d = ", dim),
# #                       levels = paste("d = ", unique(dim)))) %>%
# #   ggplot( aes(x = ntrain, y = MSE, color = Estimator)) +
# #   theme_bw(base_size = 10, base_family = "Helvetica") +
# #   geom_line() +
# #   geom_point(aes(shape = Estimator), size = 1.5) +
# #   #  scale_shape_manual(values=LETTERS[1:26])  +
# #   # coord_cartesian(ylim = c(0, .4), xlim = c(0,2000)) +
# #   theme(legend.position="bottom") +
# #   scale_y_sqrt() +
# #   ylab("EMSE") +
# #   facet_wrap(~dimSet, scales = "free", shrink = FALSE, ncol = 3) +
# #   xlab("Number of training samples")
# #
# # ggsave(paste0("~/Dropbox/CATE/Xmodel/figures/Sim12.pdf"), height = 4.6, width = 5.9)
# #
# #
# #
# #
# #
# #
# # pEMSE <- function(csetup, cdim, lb, ub){
# #   return(
# #   Results %>% filter(setup %in% c("TTMpp", "STMpp", "Conf2", "Usual4", "rare1"),
# #                    Estimator %in% c("S_RF", "X_RF", "X_RF_sRF", "T_RF", "U_RF")) %>%
# #   mutate(setup = as.character(setup),
# #          setup = ifelse(setup == "TTMpp", "Simulation 1", setup),
# #          setup = ifelse(setup == "STMpp",  "Simulation 2", setup),
# #          setup = ifelse(setup == "Conf2",  "Simulation 5", setup),
# #          setup = ifelse(setup == "rare1",  "Simulation 4", setup),
# #          setup = ifelse(setup == "Usual4",  "Simulation 3", setup)) %>%
# #   filter(setup == paste("Simulation", csetup) & dim == cdim) %>%
# #   mutate(dim = paste("d = ", dim),
# #          dimSet = paste0(setup, ", ", dim)) %>%
# #   ggplot( aes(x = ntrain, y = MSE, color = Estimator)) +
# #   theme_bw(base_size = 10, base_family = "Helvetica") +
# #   geom_line() +
# #   geom_point(aes(shape = Estimator), size = 1.5) +
# #   #  scale_shape_manual(values=LETTERS[1:26])  +
# #   # coord_cartesian(ylim = c(0, .4), xlim = c(0,2000)) +
# #   theme(legend.position="none") +
# #   scale_y_sqrt() +
# #   ylab("") +
# #   coord_cartesian(ylim = c(lb, ub)) +
# #   xlab("") +
# #   ggtitle("dimSet")
# #   )
# # }
# #
# # grid.newpage()
# # g <- grid.draw(rbind(
# #   cbind(ggplotGrob(pEMSE(1,5,0,1)), ggplotGrob(pEMSE(1,20,0,1)), ggplotGrob(pEMSE(1,100,0,1)),size = "last"),
# #   cbind(ggplotGrob(pEMSE(2,5,0,1)), ggplotGrob(pEMSE(2,20,0,1)), ggplotGrob(pEMSE(2,100,0,1)),size = "last"),
# #   cbind(ggplotGrob(pEMSE(3,5,0,1)), ggplotGrob(pEMSE(3,20,0,1)), ggplotGrob(pEMSE(3,100,0,1)),size = "last"),
# #   cbind(ggplotGrob(pEMSE(4,5,0,1)), ggplotGrob(pEMSE(4,20,0,1)), ggplotGrob(pEMSE(4,100,0,1)),size = "last"),
# #   cbind(ggplotGrob(pEMSE(5,5,0,1)), ggplotGrob(pEMSE(5,20,0,1)), ggplotGrob(pEMSE(5,100,0,1)),size = "last"), size = "last"))
# # ggsave(paste0("~/Dropbox/CATE/Xmodel/figures/Sim12345.pdf"), height = 4, width = 5.9, plot = g, dpi = 100)
# # ?ggsave
# #
# #
#
#
# # forth try:
# ds <-
#   Results %>%  filter(
#     setup %in% c("TTMpp5", "STMpp", "Conf2", "rare1"),
#     Estimator %in% c("S_RF", "X_RF", "X_RF_sRF", "T_RF", "U_RF")
#   ) %>%
#   filter(!(Estimator == "U_RF" & dim == 5 & setup == "Conf2")) %>%
#   # filter((!Estimator == "X_RF_sRF") | ((Estimator == "X_RF_sRF") & (setup == "Usual4"))) %>%
#   # filter((!setup == "STMpp") | ((setup == "STMpp") & (ntrain > 1000))) %>%
#   filter(ntrain > 500) %>%
#   mutate(
#     setup = as.character(setup),
#     setup = ifelse(setup == "TTMpp5",  "S1 - complex CATE", setup),
#     setup = ifelse(setup == "STMpp",  "S2 - CATE = 0", setup),
#     setup = ifelse(setup == "Conf2",  "S3 - Confounding", setup),
#     setup = ifelse(setup == "rare1",  "S4 - Few Treated", setup),
#     setup = ifelse(setup == "Usual4",  "Simulation 5", setup)
#   ) %>%
#   mutate(dim = ifelse(dim == 30, 100, dim)) %>%
#   filter(dim %in% c(5, 20, 100)) %>%
#   mutate(dim = factor(paste("d = ", dim),
#                       levels = paste("d = ", unique(dim)))) %>%
#   filter(is.finite(MSE)) %>%
#   filter(ntrain <= 10000)
#
# inner_join(ds,
#            ds %>% group_by(dim, setup) %>% summarize(
#              MSE_ma = max(MSE, na.rm = TRUE),
#              MSE_mi = min(MSE, na.rm = TRUE)
#            )) %>%
#   mutate(MSE = (MSE - MSE_mi) / (MSE_ma - MSE_mi)) %>%
#   ggplot(aes(
#     x = ntrain,
#     y = MSE,
#     color = Estimator,
#     linetype = Estimator
#   )) +
#   theme_bw(base_size = 10, base_family = "Helvetica") +
#   geom_line() +
#   geom_point(aes(shape = Estimator), size = 1.5) +
#   theme(legend.position = "bottom") +
#   ylab("") +
#   facet_grid(
#     setup ~ dim,
#     scales = "free",
#     space = "free_x",
#     shrink = FALSE,
#     switch = "y"
#   ) +
#   xlab("Number of training samples") +
#   coord_cartesian(ylim = c(0, .8)) +
#   scale_color_manual(values = cols) +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank()) #+
# # geom_errorbar(aes(ymin = MSE - 2 * MSE.sd, ymax = MSE + 2 * MSE.sd), width=100)
# # ggsave(paste0("~/Dropbox/CATE/Xmodel/figures/SimPaper.pdf"), height = 24, width = 24)
#
# ggsave(
#   paste0("~/Dropbox/CATE/Xmodel/figures/SimPaper.pdf"),
#   height = 6,
#   width = 5.9
# )
#
# ggplot_build(pp)$data
