library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 90 # 100

################
# Type I
################
shapes <- seq(1, 9, 2)
rates <- seq(1, 9, 2)

sim_results <- tibble()
for (shape in shapes) {
  for (rate in rates) {
    scale <- 1 / rate

    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "gamma_rate_one_way"
    for (i in 1:B) {
      set.seed(i)
      x <- rgamma(n = N, shape = shape, rate = rate)
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- gamma_rate_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, shape = shape, rate = rate, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr)
  }
}

sim_results %>% saveRDS("results/gamma_type_one_rate_one_way.rds")

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(shape) %>%
  nrow() == length(shapes)

sim_results %>%
  distinct(rate) %>%
  nrow() == length(rates)

sim_results %>%
  distinct(scale) %>%
  nrow() == length(rates)

sim_results %>%
  distinct(alt) %>%
  nrow() == 1

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

rm(list = ls())
