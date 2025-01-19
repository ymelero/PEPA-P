# -------------------------------------------------------------------------
# SCRIPT FOR BOX 1
# Author: [Yolanda Melero]
# Project: PEPA
# -------------------------------------------------------------------------

# Load necessary libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(123)
n <- 100000  # Sample size for smooth curves

# Panel 1: A single standard normal distribution
x1 <- rnorm(n, mean = 0, sd = 1.5)

# Calculate mean and standard deviation
mean_x1 <- mean(x1)
sd_x1 <- sd(x1)
threshold <- mean_x1 + 1 * sd_x1  # Define the threshold

# Create the density data
density_data <- density(x1, adjust = 1.6)
density_df <- data.frame(x = density_data$x, y = density_data$y)

# Split the data into two parts: below and above the threshold
density_df$color <- ifelse(density_df$x > threshold, "dark", "light")

# Create the plot
ggplot(density_df, aes(x = x, y = y)) +
  geom_area(data = subset(density_df, color == "light"),
            fill = "#F4C27A",  alpha = 0.5) +  # Light orange
  geom_area(data = subset(density_df, color == "dark"),
            fill = "#F19A3E",  alpha = 0.9) +  # Dark orange
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    strip.text = element_blank()
  ) +
  coord_cartesian(xlim = c(-5, 5))  # Set x-axis range

### 
# Load necessary libraries
library(ggplot2)
library(sn)

# Set seed for reproducibility
set.seed(123)
n_clara <- 100000  # Sample size for smooth curves

# Generate the wider left-skewed distribution (clara)
x_clara <- rsn(n_clara, xi = -1, omega = 2.5, alpha = 5)  # Wider and left-skewed
density_clara <- density(x_clara, adjust = 1.6)
density_clara_df <- data.frame(x = density_clara$x, y = density_clara$y)

# Generate the right-skewed distribution (oscura)
x_oscura <- rsn(n_clara, xi = 2, omega = 1.5, alpha = -8)  # Right-skewed
density_oscura <- density(x_oscura, adjust = 1.6)
density_oscura_df <- data.frame(x = density_oscura$x, y = density_oscura$y)

# Create the plot for the clear distribution
plot_clara <- ggplot(density_clara_df, aes(x = x, y = y)) +
  geom_area(fill = "#F4C27A", alpha = 0.8) +  # Light orange
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    strip.text = element_blank()
  ) +
  coord_cartesian(xlim = c(-6, 6))  # Set x-axis range

# Create the plot for the dark distribution
plot_oscura <- ggplot(density_oscura_df, aes(x = x, y = y)) +
  geom_area(fill = "#F19A3E", alpha = 0.9) +  # Dark orange
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    strip.text = element_blank()
  ) +
  coord_cartesian(xlim = c(-6, 6))  # Set x-axis range

# Print both plots
print(plot_clara)
print(plot_oscura)
