library(LRTesteR)
library(tidyverse)
library(stringr)
library(statmod)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 70 # 70

################
# Type I
################
mus <- seq(1, 9, 2)
shapes <- seq(1, 9, 2)

sim_results <- tibble()
for (mu in mus) {
  for (shape in shapes) {
    dispersion <- 1 / shape
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "inverse_gaussian_mu_one_way"
    for (i in 1:B) {
      set.seed(i)
      x <- rinvgauss(n = N, mean = mu, shape = shape)
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- inverse_gaussian_mu_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, mu = mu, shape = shape, dispersion = dispersion, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)
  }
}

for (mu in mus) {
  for (shape in shapes) {
    dispersion <- 1 / shape
    
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "inverse_gaussian_shape_one_way"
    for (i in 1:B) {
      set.seed(i)
      x <- rinvgauss(n = N, mean = mu, shape = shape)
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- inverse_gaussian_shape_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, mu = mu, shape = shape, dispersion = dispersion, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)
  }
}

dispersions <- seq(1, 9, 2)
for (mu in mus) {
  for (dispersion in dispersions) {
    shape <- 1 / dispersion
    
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "inverse_gaussian_dispersion_one_way"
    for (i in 1:B) {
      set.seed(i)
      x <- rinvgauss(n = N, mean = mu, dispersion = dispersion)
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- inverse_gaussian_dispersion_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, mu = mu, shape = shape, dispersion = dispersion, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 3

sim_results %>%
  distinct(mu) %>%
  nrow() == length(mus)

sim_results %>%
  distinct(shape) %>%
  nrow() >= length(shapes)

sim_results %>%
  distinct(dispersion) %>%
  nrow() >= length(dispersions)

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
  saveRDS("results/inverse_gaussian_type_one_one_way.rds")

rm(list = ls())
