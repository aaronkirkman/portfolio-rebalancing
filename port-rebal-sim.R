library(magrittr)
library(sm)
library(tidyverse)

# Setup -------------------------------------------------------------------
setwd("/home/ak/Dropbox/projects/portfolio-rebalancing")
set.seed(1)

# Parameters --------------------------------------------------------------
num_periods <- 100

# Assets ------------------------------------------------------------------
shocks <- c(
  function() rnorm(1, 0.025, 0.04), # "equity" asset
  function() rnorm(1, 0.01, 0.01) # "bond" asset
)

# Allocation --------------------------------------------------------------
target_allocation <- c(0.90, 0.10)

# Utility Functions -------------------------------------------------------
portfolio_return <- function(portfolio_values) {
  return(sum(portfolio_values[nrow(portfolio_values), ]) / sum(portfolio_values[1, ]) - 1)
}

rmse <- function(vector1, vector2) {
  return(sqrt(sum(abs(vector1 - vector2)^2)))
}

# Simulation Functions--------------------------------------------------------------
model_no_rebalancing <- function(target_allocation, num_periods, shocks) {
  # target_allocation: The ideal allocation for the portfolio. In this model
  # (with no rebalancing), this is only used to initialize the portfolio

  # num_periods: The number of periods to run the simulation

  # shocks: A vector of functions representing the probability distributions
  # from which the returns are drawn
  portfolio <- matrix(nrow = num_periods, ncol = length(target_allocation))
  portfolio[1, ] <- target_allocation
  for (t in 2:num_periods) {
    returns <- sapply(shocks, function(f) f())
    portfolio[t, ] <- portfolio[t-1, ] * (1 + returns)
  }
  return(portfolio_return(portfolio))
}

model_cyclical_rebalancing <- function(target_allocation, num_periods, rebalance_every, shocks) {
  # target_allocation: The ideal allocation for the portfolio. Rebalancing brings
  # the portfolio back to this allocation

  # num_periods: The number of periods to run the simulation

  # rebalance_every: Rebalance every time this many periods have elapsed, AFTER the portfolio
  # value is calculated using the randomly-drawn returns for that period

  # shocks: A vector of functions representing the probability distributions
  # from which the returns are drawn
  portfolio <- matrix(nrow = num_periods, ncol = length(target_allocation))
  portfolio[1, ] <- target_allocation
  for (t in 2:num_periods) {
    returns <- sapply(shocks, function(f) f())
    portfolio[t, ] <- portfolio[t-1, ] * (1 + returns)
    if (t %% rebalance_every == 0) {
      portfolio[t, ] <- sum(portfolio[t, ]) * target_allocation
    }
  }
  return(portfolio_return(portfolio))
}

model_rmse_rebalancing <- function(target_allocation, num_periods, rmse_threshold, shocks) {
  portfolio <- matrix(nrow = num_periods, ncol = length(target_allocation))
  portfolio[1, ] <- target_allocation
  for (t in 2:num_periods) {
    returns <- sapply(shocks, function(f) f())
    portfolio[t, ] <- portfolio[t-1, ] * (1 + returns)
    if (rmse(target_allocation, portfolio[t, ]) > rmse_threshold) {
      portfolio[t, ] <- sum(portfolio[t, ]) * target_allocation
    }
  }
  return(portfolio_return(portfolio))
}


# Simulation --------------------------------------------------------------
num_simulation_runs <- 50
simulation_results <- matrix(nrow = num_simulation_runs, ncol = 3)
for (i in 1:num_simulation_runs) {
  print(sprintf("Simulation #%d", i))
  simulation_results[i, 1] = model_no_rebalancing(target_allocation, num_periods, shocks)
  simulation_results[i, 2] = model_cyclical_rebalancing(target_allocation, num_periods, rebalance_every = 4, shocks)
  simulation_results[i, 3] = model_rmse_rebalancing(target_allocation, num_periods, rmse_threshold = 0.05, shocks)
}
colnames(simulation_results) <- c("None", "Cylical", "RMSE")
results <- as_tibble(simulation_results, .rows = num_simulation_runs) %>%
  pivot_longer(cols = c(None, Cylical, RMSE), names_to = "method", values_to = "return") %>%
  mutate(method = as.factor(method))

# Plots and Kernels -------------------------------------------------------
results %$%
  sm.density.compare(return, method)
title(main="Portfolio Returns by Rebalancing Method")
colfill<-c(2:(2+length(levels(results$method))))
legend(x=18, y=0.10, levels(results$method), fill=colfill)
