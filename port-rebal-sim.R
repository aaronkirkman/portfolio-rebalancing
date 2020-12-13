# Setup -------------------------------------------------------------------
setwd("/home/ak/Dropbox/projects/portfolio-rebalancing")
set.seed(1)

# Parameters --------------------------------------------------------------
num_periods <- 100
rebalance_every <- 4

# Assets ------------------------------------------------------------------
shocks <- c(
  function() rnorm(1, 0.025, 0.04), # "equity" asset
  function() rnorm(1, 0.01, 0.01) # "bond" asset
)

# Allocation --------------------------------------------------------------
target_allocation <- c(0.90, 0.10)

# Simulation --------------------------------------------------------------
with_reb <- matrix(nrow = num_periods, ncol = length(shocks))
without_reb <- matrix(nrow = num_periods, ncol = length(shocks))

with_reb[1, ] <- target_allocation
without_reb[1, ] <- target_allocation


for (t in 2:num_periods) {
  returns <- sapply(shocks, function(f) f())
  without_reb[t, ] <- without_reb[t-1, ] * (1 + returns)
  with_reb[t, ] <- with_reb[t-1, ] * (1 + returns)
  if (t %% rebalance_every == 0) {
    with_reb[t, ] <- sum(with_reb[t, ]) * target_allocation
  }
}

total_portfolio_return <- function(portfolio_values) {
  N <- nrow(portfolio_values)
  return(sum(portfolio_values[N, ]) / sum(portfolio_values[1, ]) - 1)
}

sprintf("%3.2f%%", total_portfolio_return(without_reb) * 100)
sprintf("%3.2f%%", total_portfolio_return(with_reb) * 100)
