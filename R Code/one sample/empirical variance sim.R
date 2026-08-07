library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 15 # 15

################
# Type I
################
variances <- 1

# The sample must have wide enough range.
# Otherwise, the optimization behind the test cannot be solved.
# If the optimization cannot be solved, take another sample.
not_testable_x <- function(x, null_variance) {
  result <- (max(x) - min(x)) / 4 < variance
  return(result)
}

sim_results <- tibble()
for (variance in variances) {
  for (alt in c("two.sided", "less", "greater")) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    CI_LBs <- vector(mode = "numeric", length = B)
    CI_UBs <- vector(mode = "numeric", length = B)
    testName <- "empirical_variance_test"
    for (i in 1:B) {
      set.seed(i)
      x <- rnorm(n = N, mean = 0, sd = variance^.5)
      while (not_testable_x(x, variance)) {
        x <- rnorm(n = N, mean = 0, sd = variance^.5)
      }
      test <- empirical_variance_test(x, variance, alt)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
      CI_LBs[i] <- test$conf.int[1]
      CI_UBs[i] <- test$conf.int[2]
    }
    temp <- tibble(test = testName, mu = 0, variance = variance, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
    sim_results <- sim_results |> bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, test, x, CI_LBs, CI_UBs)
  }
}

# Check structure
sim_results |>
  distinct(test) |>
  nrow() == 1

sim_results |>
  distinct(variance) |>
  nrow() == length(variances)

sim_results |>
  distinct(alt) |>
  nrow() == 3

sim_results |>
  pull(pvalue) |>
  min(na.rm = TRUE) >= 0

sim_results |>
  pull(pvalue) |>
  max(na.rm = TRUE) <= 1

sim_results |>
  filter(alt == "two.sided") |>
  summarise(CICheck = all(CI_LB < CI_UB))

# save
sim_results |>
  saveRDS("results/empirical_variance_type_one.rds")

rm(list = ls())
