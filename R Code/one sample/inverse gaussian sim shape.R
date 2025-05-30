library(LRTesteR)
library(tidyverse)
library(stringr)
library(statmod)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 35 # 35

################
# Type I
################
mus <- seq(1, 9, 2)
shapes <- seq(1, 9, 2)

sim_results <- tibble()
for (mu in mus) {
  for (shape in shapes) {
    dispersion <- 1 / shape
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      CI_LBs <- vector(mode = "numeric", length = B)
      CI_UBs <- vector(mode = "numeric", length = B)
      testName <- "inverse_gaussian_shape_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rinvgauss(n = N, mean = mu, shape = shape)
        test <- inverse_gaussian_shape_one_sample(x, shape, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
        if (is.na(test$conf.int[1]) | is.na(test$conf.int[2])) {
          msg <- str_c("i:", i, " alt:", alt, " mu:", mu, " shape:", shape)
        }
      }
      temp <- tibble(test = testName, mu = mu, shape = shape, dispersion = dispersion, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i, test, x, CI_LBs, CI_UBs)
    }
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(mu) %>%
  nrow() == length(mus)

sim_results %>%
  distinct(shape) %>%
  nrow() == length(shapes)

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
  saveRDS("results/inverse_gaussian_type_one_shape.rds")

rm(list = ls())
