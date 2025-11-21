library(tidyverse)
library(gganimate)
library(magick)
library(png)
library(glue)

# PARAMATERS OF THE EXPERIMENT
beta_0 <- 1; beta_1 <- 2
M <- 5000 #Number of experiments
N <- 200 #Number of maximum observations
sigma_2 <- 64
max_unif <- 10; min_unif <- -10

lambda <- 1/sqrt(sigma_2) #Definition of exp.

x <- runif(N, min = min_unif, max=max_unif)

montecarlo <- sapply(1:M, function(m){
  eps <- rexp(N, lambda)
  y <- beta_0 + beta_1*x + eps
  data <- as.data.frame(cbind(x, eps, y, n=1:N))
  sapply(2:N, function(n){
    data_n <- data[which(data[, "n"] <= n),]
    
    X_n <- data_n[, "x"]
    X_n_bar <- mean(X_n)

    Y_n <- data_n[, "y"]
    Y_n_bar <- mean(Y_n)

    ols_beta_1 <- sum((Y_n - Y_n_bar)*(X_n - X_n_bar))/sum((X_n - X_n_bar)^2)
    ols_beta_1

  })
})

montecarlo_df <- as.data.frame(t(montecarlo), row.names = 1:M)
colnames(montecarlo_df) <- paste0("n_", 2:N)

montecarlo_long <- montecarlo_df |>
  mutate(experiment = row_number()) |>
  pivot_longer(cols = starts_with("n_"), names_to = "n", values_to = "ols_estimator") %>%
  mutate(n = as.integer(sub("n_", "", n)))

#Save as csv
write_csv(montecarlo_long, "ols_montecarlo_simulation.csv")

