library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 30 # 30

################
# Type I
################
mu <- 0
variances <- 1

sim_results <- tibble()
for (variance in variances) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "empirical_variance_one_way_test"
  for (i in 1:B) {
    set.seed(i)
    x <- rnorm(n = N, mean = mu, sd = variance^.5)
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- empirical_variance_one_way_test(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results |> bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)
}

# Check structure
sim_results |>
  distinct(test) |>
  nrow() == 1

sim_results |>
  distinct(variance) |>
  nrow() == length(variances)

sim_results |>
  distinct(mu) |>
  nrow() == 1

sim_results |>
  distinct(alt) |>
  nrow() == 1

sim_results |>
  pull(pvalue) |>
  min(na.rm = TRUE) >= 0

sim_results |>
  pull(pvalue) |>
  max(na.rm = TRUE) <= 1

# save
sim_results |>
  saveRDS("results/empirical_variance_type_one_one_way.rds")

rm(list = ls())
