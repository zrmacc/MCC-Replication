# Purpose: Function to plot the mean cumulative functions,
# comparing two treatment arms.

library(cowplot)
library(ggplot2)
library(MCC)

#' Plot MCFs.
#'
#' @param ctrl_color Color for control arm.
#' @param data Data including time, status, idx, arm.
#' @param title Plot title.
#' @param trt_color Color for treatment arm.
#' @param x_max X-axis upper limit.
#' @param y_lim Y-axis limits.
#' @return ggplot.

PlotMCFs <- function(
  ctrl_color = "#C65842",
  data,
  title,
  trt_color = "#6385B8",
  x_max = NULL,
  y_lim = NULL
) {
  
  # Split data.
  data0 <- subset(
    x = data,
    arm == 0
  )
  data1 <- subset(
    x = data,
    arm ==1 
  )
  
  # Estimate mean cumulative function (MCF).
  fit_mcf_0 <- MCC::CalcMCF(
    time = data0$time,
    status = data0$status,
    idx = data0$idx
  )
  
  fit_mcf_1 <- MCC::CalcMCF(
    time = data1$time,
    status = data1$status,
    idx = data1$idx
  )
  
  # MCF function for arm 0
  g0 <- stepfun(
    x = fit_mcf_0$time,
    y = c(0, fit_mcf_0$mcf)
  )
  
  # MCF function for arm 1
  g1 <- stepfun(
    x = fit_mcf_1$time,
    y = c(0, fit_mcf_1$mcf)
  )
  
  # Plotting frame for control arm.
  df0 <- df1 <- data.frame(
    "time" = seq(from = 1, to = x_max, length.out = 200)
  )
  df0$mcf <- g0(df0$time)
  df0$arm <- 0
  
  # Plotting frame for treatment arm.
  df1$mcf <- g1(df1$time)
  df1$arm <- 1
  
  df <- rbind(df0, df1)
  df$arm <- factor(df$arm, levels = c(0, 1))
  
  # Plotting.
  q <- ggplot() +
    theme_bw() + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = c(0.2, 0.8)
    ) + 
    geom_step(
      data = df, 
      aes(x = time, y = mcf, color = arm), 
      size = 1) + 
    scale_color_manual(
      name = NULL,
      values = c(ctrl_color, trt_color),
      labels = c("Placebo", "Thiotepa")
    ) + 
    labs(
      x = "Time",
      y = "Mean Cumulative Count"
    ) +
    lims(
      y = y_lim
    ) + 
    ggtitle(
      label = title
    )
  
  # Output.
  return(q)
}