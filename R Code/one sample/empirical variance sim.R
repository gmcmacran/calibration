library(LRTesteR)
library(tidyverse)
library(stringr)
library(furrr)

################
# Simulation settings
################
compiler::enableJIT(3)
plan(multisession, workers = 10)
B <- 5000
N <- 15 # 15

################
# Type I
################
variances <- seq(.5, 3, .50)

run_sim <- function(variances) {
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
        test <- empirical_variance_test(x, variance, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
      }
      temp <- tibble(test = testName, mu = 0, variance = variance, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i, test, x, CI_LBs, CI_UBs)
    }
  }
  return(sim_results)
}

sim_results <- future_map_dfr(variances, run_sim, .options = furrr_options(seed = TRUE))

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(variance) %>%
  nrow() == length(variances)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

sim_results %>%
  filter(alt == "two.sided") %>%
  summarise(CICheck = all(CI_LB < CI_UB))

# save
sim_results %>%
  saveRDS("results/empirical_variance_type_one.rds")

plan(sequential)
rm(list = ls())
