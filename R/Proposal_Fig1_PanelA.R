# -------------------------------------------------------------------------
# SCRIPT FOR FIG 1 PANNEL A
# Author: [Yolanda Melero]
# Project: PEPA
# -------------------------------------------------------------------------

# Load necessary libraries
library(ggplot2)
library(sn)

# Set seed for reproducibility
set.seed(123)
n <- 100000  # Sample size for smooth curves
n_small <- 20000  # Reduced sample size for µ2 in Panel 2

# Panel 1: A single standard normal distribution
x1 <- rnorm(n, mean = 0, sd = 1.5)

# Panel 2: Combined distribution (S2 as in the first code)
x2_1 <- rnorm(n, mean = 0, sd = 1.5)     # µ1: rounded
x2_2 <- rnorm(n_small, mean = 0, sd = 0.6)  # µ2: narrower and less dense

# Panel 3: µ1 normal and µ2 lower and skewed to the left
x3_1 <- rnorm(n, mean = 0, sd = 1.5)  # µ1: standard normal
x3_2 <- rsn(n, xi = 3, omega = 1.5, alpha = -3)  # µ2: skewed to the left

# Panel 4 (New): XX_2 bimodal with peaks at the extremes
x4_1 <- rnorm(n, mean = 0, sd = 1.5)  # µ1: standard normal
x4_2 <- c(rnorm(n_small / 2, mean = -2.5, sd = 0.16), rnorm(n_small / 2, mean = 2.5, sd = 0.16))  # µ2: bimodal

# Panel 5: Two overlapping normal distributions
x5_1 <- rnorm(n, mean = -1, sd = 1.5)
x5_2 <- rnorm(n, mean = 1, sd = 1.5)

# Create data frames for each panel
data1 <- data.frame(value = x1, group = "µ", panel = "Panel 1")
data2 <- data.frame(value = c(x2_1, x2_2),
                    group = rep(c("µ1", "µ2"), times = c(n, n_small)),
                    panel = "Panel 2")
data3 <- data.frame(value = c(x3_1, x3_2),
                    group = rep(c("µ1", "µ2"), each = n),
                    panel = "Panel 3")
data4 <- data.frame(value = c(x4_1, x4_2),
                    group = rep(c("µ1", "µ2"), times = c(n, n_small)),
                    panel = "Panel 4")
data5 <- data.frame(value = c(x5_1, x5_2),
                    group = rep(c("µ1", "µ2"), each = n),
                    panel = "Panel 5")

# Combine all data
data <- rbind(data1, data2, data3, data4, data5)

# Change the order of panel levels as requested
data$panel <- factor(data$panel, levels = c("Panel 1", "Panel 2", "Panel 3", "Panel 4", "Panel 5"))

# Define colors
colors <- c("µ" = "#CFCFCF", "µ1" = "#A6CEE3", "µ2" = "#FB9A99", "µ1,2" = "#80B1D3")

# Create the final plot
ggplot(data, aes(x = value, fill = group)) +
  geom_density(data = subset(data, panel == "Panel 2" & group != "µ2"),  
               alpha = 0.8, color = "white", adjust = 1.6) +
  geom_density(data = subset(data, panel == "Panel 2" & group == "µ2"),  
               aes(y = ..density.. * 0.5),  
               alpha = 0.8, color = "white", adjust = 1.6) +
  geom_density(data = subset(data, panel != "Panel 2"),  
               alpha = 0.8, color = "white", adjust = 1.6) +
  geom_vline(data = data.frame(
    panel = c("Panel 1", "Panel 2", "Panel 2", "Panel 3", "Panel 3", "Panel 4", "Panel 4", "Panel 5", "Panel 5"),
    xintercept = c(0, 0, 0, mean(x3_1), mean(x3_2), -0, 0, -1, 1)),
    aes(xintercept = xintercept),
    linetype = "dashed", color = "grey70", size = 0.2) +
  facet_wrap(~panel, scales = "free_y", ncol = 5) +
  scale_fill_manual(values = colors) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(-0.6, "cm")  # Reduce spacing between panels
  ) +
  coord_cartesian(xlim = c(-5, 5))  # Unify the range of all plots
