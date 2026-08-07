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
N_loc <- 30 # 30
N_scale <- 70 # 70

################
# Type I
################
locations <- seq(-4, 4, 2)
scales <- seq(1, 9, 2)

run_sim <- function(locations) {
  sim_results <- tibble()
  for (location in locations) {
    for (scale in scales) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "cauchy_location_one_way_test"
      for (i in 1:B) {
        set.seed(i)
        x <- rcauchy(N_loc, location, scale)
        fctr <- factor(c(rep("1", N_loc / 2), rep("2", N_loc / 2)), levels = c("1", "2"))
        test <- cauchy_location_one_way_test(x, fctr)

        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, location = location, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)

      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "cauchy_scale_one_way_test"
      for (i in 1:B) {
        set.seed(i)
        x <- rcauchy(N_scale, location, scale)
        fctr <- factor(c(rep("1", N_scale / 2), rep("2", N_scale / 2)), levels = c("1", "2"))
        test <- cauchy_scale_one_way_test(x, fctr)

        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, location = location, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)
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
  nrow() == 1

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/cauchy_type_one_one_way.rds")

plan(sequential)
rm(list = ls())
