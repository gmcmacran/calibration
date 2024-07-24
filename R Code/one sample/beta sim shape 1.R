library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 40 # 40
# options(warn=2, error=recover)

################
# Type I
################
shape1s <- seq(1, 9, 2)
shape2s <- seq(1, 9, 2)

sim_results <- tibble()
for (shape1 in shape1s) {
  for (shape2 in shape2s) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      CI_LBs <- vector(mode = "numeric", length = B)
      CI_UBs <- vector(mode = "numeric", length = B)
      testName <- "beta_shape1_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rbeta(N, shape1 = shape1, shape2 = shape2)
        test <- beta_shape1_one_sample(x, shape1, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
      }
      temp <- tibble(test = testName, shape1 = shape1, shape2 = shape2, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 2

sim_results %>%
  distinct(shape1) %>%
  nrow() == length(shape1s)

sim_results %>%
  distinct(shape2) %>%
  nrow() == length(shape2s)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

all(sim_results$CI_LB < sim_results$CI_UB)

# save
sim_results %>%
  saveRDS("results/beta_type_one_shape1.rds")

rm(list = ls())
