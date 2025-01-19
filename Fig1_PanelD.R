# -------------------------------------------------------------------------
# SCRIPT FOR FIG. X PANEL X
# Author: [Yolanda Melero]
# Project: XX
# -------------------------------------------------------------------------

# Load necessary libraries
library(ggplot2)
library(sn)

# Set seed for reproducibility
set.seed(123)
n <- 100000  # Sample size for smooth curves
n_small <- 20000  # Reduced sample size for smaller distributions

# Community 1: Two overlapping normal distributions in blue
x1 <- rnorm(n, mean = 0, sd = 1.5)  # Species 1
y2_1 <- rnorm(n, mean = 1.5, sd = 1.5)  # Species 2

data_comm1 <- data.frame(
  value = c(x1, y2_1),
  species = rep(c("sp1", "sp2"), each = n),
  community = "Community 1"
)

# Community 2: Smaller and skewed distributions in pink
x2_2 <- rnorm(n_small, mean = 0, sd = 0.6)  # Species 3
x2_3 <- rsn(n_small, xi = 3, omega = 1.5, alpha = -3)  # Species 4 (skewed to the left)

data_comm2 <- data.frame(
  value = c(x2_2, x2_3),
  species = rep(c("sp3", "sp4"), each = n_small),
  community = "Community 2"
)

# Combine data for both communities
data <- rbind(data_comm1, data_comm2)
data$community <- relevel(factor(data$community), ref = "Community 1")

# Calculate weighted means for vertical dashed lines
mean_comm1 <- mean(data_comm1$value)
mean_comm2 <- mean(data_comm2$value)

# Define colors
colors <- c("sp1" = "#A6CEE3", "sp2" = "#A6CEE3", "sp3" = "#FB9A99", "sp4" = "#FB9A99")

# Create the plot
ggplot(data, aes(x = value, fill = species)) +
  geom_density(data = subset(data, community == "Community 1"), alpha = 0.8, color = "white", adjust = 1.6) +
  geom_density(data = subset(data, community == "Community 2"), alpha = 0.8, color = "white", adjust = 1.6) +
  geom_vline(data = data.frame(community = c("Community 1", "Community 2"), mean = c(mean_comm1, mean_comm2)),
             aes(xintercept = mean, group = community), linetype = "dotted", color = "grey50", size = 0.2) + facet_wrap(~community, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = colors) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    strip.text = element_text(size = 14, family = "Times New Roman", hjust = 0.6, margin = margin(b = 10))
  ) +
  coord_cartesian(xlim = c(-5, 5))
