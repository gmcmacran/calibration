library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 40

################
# Type I
################
shape1s <- seq(1, 9, 4)
shape2s <- seq(1, 9, 4)

sim_results <- tibble()
for (shape1 in shape1s) {
  for (shape2 in shape2s) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "beta_shape2_one_way"
    for (i in 1:B) {
      set.seed(i)
      x <- rbeta(N, shape1 = shape1, shape2 = shape2)
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- beta_shape2_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, shape1 = shape1, shape2 = shape2, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)
  }
}
rm(shape1, shape2)

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(shape1) %>%
  nrow() == length(shape1s)

sim_results %>%
  distinct(shape2) %>%
  nrow() == length(shape2s)

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
  saveRDS("results/beta_type_one_one_way_shape2.rds")

rm(list = ls())
