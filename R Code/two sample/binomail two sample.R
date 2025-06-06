library(LRTesteR)
library(tidyverse)
library(stringr)
library(lmtest)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 50 # 50

################
# Type I
################
ps <- seq(.05, .95, .10)

all(ps < 1)
all(ps > 0)

sim_results <- tibble()
for (p in ps) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "binomial_p_one_way"
  for (i in 1:B) {
    set.seed(i)
    Ns <- rep(N / 2, 2)
    x <- rbinom(2, Ns, p)
    fctr <- factor(c(rep("1", length(x) / 2), rep("2", length(x) / 2)), levels = c("1", "2"))
    test <- binomial_p_one_way(x, Ns, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, p = p, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, Ns, test, x)
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(p) %>%
  nrow() == length(ps)

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
  saveRDS("results/binomail_type_one_one_way.rds")

rm(list = ls())
