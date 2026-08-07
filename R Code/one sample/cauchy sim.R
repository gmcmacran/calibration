library(LRTesteR)
library(tidyverse)
library(stringr)
library(furrr)

################
# Simulation settings
################
compiler::enableJIT(3)
plan(multisession, workers = 5)
B <- 5000
N_loc <- 15 # 15
N_scale <- 35 # 35

################
# Type I
################
locations <- seq(-4, 4, 2)
scales <- seq(1, 9, 2)

run_sim <- function(locations) {
  sim_results <- tibble()
  for (location in locations) {
    for (scale in scales) {
      for (alt in c("two.sided", "less", "greater")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        CI_LBs <- vector(mode = "numeric", length = B)
        CI_UBs <- vector(mode = "numeric", length = B)
        testName <- "cauchy_location_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rcauchy(N_loc, location, scale)
          test <- cauchy_location_test(x, location, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
          CI_LBs[i] <- test$conf.int[1]
          CI_UBs[i] <- test$conf.int[2]
        }
        temp <- tibble(test = testName, location = location, scale = scale, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }

      for (alt in c("two.sided", "less", "greater")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        CI_LBs <- vector(mode = "numeric", length = B)
        CI_UBs <- vector(mode = "numeric", length = B)
        testName <- "cauchy_scale_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rcauchy(N_scale, location, scale)
          test <- cauchy_scale_test(x, scale, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
          CI_LBs[i] <- test$conf.int[1]
          CI_UBs[i] <- test$conf.int[2]
        }
        temp <- tibble(test = testName, location = location, scale = scale, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    }
  }
  return(sim_results)
}

sim_results <- future_map_dfr(locations, run_sim, .options = furrr_options(seed = TRUE))

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 2

sim_results %>%
  distinct(location) %>%
  nrow() == length(locations)

sim_results %>%
  distinct(scale) %>%
  nrow() == length(scales)

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
  saveRDS("results/cauchy_type_one.rds")

rm(sim_results, locations, scales)

plan(sequential)
rm(list = ls())
