library(tidyverse)
library(gganimate)

set.seed(12345)

lindeberg_empirical <- function(x, eps = 1, max_n = 500000) {
  
  n_vals <- round(seq(100, max_n, by=100))
  results <- numeric(max_n / 100 - 1)
  
  for (i in seq_along(n_vals)) {
    n <- n_vals[i]
    samp <- sample(x, n, replace = TRUE)
    
    mu <- mean(samp)
    s2 <- var(samp)
    thresh <- eps * sqrt(n * s2)
    
    Ln <- sum((samp - mu)^2 * (abs(samp - mu) > thresh)) / (n * s2)
    results[i] <- Ln
  }
  
  tibble(n = n_vals, Ln = results)
}

# Example usage:
eps = 0.2
data <- read_csv("dblp_citations.csv")$cites
res <- lindeberg_empirical(data, eps = eps)

#Use gganimate to show the convergence of the empirical Lindeberg condition
p <- ggplot(res, aes(x = n, y = Ln)) +
  geom_line() +
  geom_point() +
  labs(title = glue::glue("Empirical Lindeberg Condition Convergence (ε = {eps})"), 
       x = "Sample Size (n)", 
       y = "Lindeberg Condition (L1)") +
  theme(text = element_text(size = 20)) +    
  transition_reveal(n)

#Use ImageMagick to save the animation
anim <- animate(p, nframes = 100, fps = 10, width = 800, height = 550)
anim_save("lindeberg_convergence.gif", animation = anim)

# Simulation of sampling distribution of sample mean for different sample sizes
B <- 5000
mu <- mean(data)

mc_all <- map_dfr(c(100, 250, 500, 1000, 10000), function(n) {
  mc_means <- replicate(B, mean(sample(data, n, replace = TRUE)) - mu)
  tibble(
    mean_hat = mc_means,
    n = n
  )
})

#Display points farther than 2 standard deviations from the mean in red
mc_all <- mc_all |>
  group_by(n) |>
  mutate(sd_hat = sd(mean_hat),
         outlier_1 = abs(mean_hat) > 5 * sd_hat,
         outlier_2 = abs(mean_hat) > 10 * sd_hat) 

sd_table <- mc_all |>
  group_by(n) |>
  summarise(
    sd_mean = sd(mean_hat),   # recompute SD for each facet
    .groups = "drop"
  )


p <- ggplot(mc_all, aes(x = mean_hat)) +
  geom_density() +
  geom_point(
    data = subset(mc_all, outlier_1), 
    aes(y = 0), 
    color = "green", size = 3, 
    shape=4
  ) +  
  geom_point(
    data = subset(mc_all, outlier_2), 
    aes(y = 0), 
    color = "red", size = 3, 
    shape=4
  ) +
  geom_vline(
    data = distinct(mc_all, n, sd_hat),
    aes(xintercept = 5 * sd_hat),
    linetype = "dashed"
  ) +
  geom_vline(
    data = distinct(mc_all, n, sd_hat),
    aes(xintercept = 10 * sd_hat),
    linetype = "dashed"
  ) +    
  geom_text(
    data = sd_table,
    aes(x = 5 * sd_mean, y = 0, label = "+5 SD"),
    color = "blue",
    hjust = -0.9,
    vjust = -0.5,
    size = 3,
    angle = 90
  ) +    
  geom_text(
    data = sd_table,
    aes(x = 10 * sd_mean, y = 0, label = "+10 SD"),
    color = "blue",
    hjust = -0.9,
    vjust = -0.5,
    size = 3,
    angle = 90
  ) +
  facet_wrap(~ n, scales = "free", ncol = 3) +
  labs(
    title = "Sampling distribution of sample mean for different n",
    x = "Sample mean",
    y = "Density"
  )

#save plot 600x400
ggsave("sampling_distribution_means.png", plot = p, width = 8, height = 6)
