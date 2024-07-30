library(LRTesteR)
library(tidyverse)

###############
# one sample
###############
load_df <- function(fn) {
  if (str_detect(fn, "two")) {
    print(str_c("Possible Error. fn: ", fn))
  }
  fn <- str_c("results/", fn, collapse = "")
  DF <- readRDS(fn)
  DF <- DF %>%
    select(test, alt, stat, pvalue, CI_LB, CI_UB)
  return(DF)
}

fns <- c(
  "gaussian_type_one.rds",
  "gaussian_type_one_exact.rds",
  "gamma_type_one_rate.rds",
  "gamma_type_one_scale.rds",
  "gamma_type_one_shape.rds",
  "poisson_type_one.rds",
  "beta_type_one_shape1.rds",
  "beta_type_one_shape2.rds",
  "negative_binomial_type_one.rds",
  "exponential_type_one.rds",
  "binomail_type_one.rds",
  "cauchy_type_one.rds",
  "inverse_gaussian_type_one_mu.rds",
  "inverse_gaussian_type_one_shape.rds",
  "inverse_gaussian_type_one_dispersion.rds",
  "empirical_mu_type_one.rds",
  "empirical_quantile_type_one.rds"
)

typeI <- map_dfr(fns, load_df)

# Some tests can produce NA in CI when sample size is small.
typeI %>%
  select(-CI_LB, -CI_UB) %>%
  drop_na() %>%
  nrow() == typeI %>%
  nrow()

typeI %>%
  distinct(test) %>%
  nrow() == 20

typeI %>%
  distinct(alt) %>%
  nrow() == 3

typeI %>%
  filter(alt == "two.sided", stat < 0, str_detect(test, "_one_sample")) %>%
  distinct(test)

typeI %>%
  filter(alt != "two.sided") %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeI %>%
  filter(alt != "two.sided") %>%
  group_by(test) %>%
  summarise(minStat = min(stat), maxStat = max(stat)) %>%
  arrange(test) %>%
  print(n = Inf)

typeI %>%
  summarise(
    P_LB = all(pvalue >= 0),
    P_UB = all(pvalue <= 1),
    CI_CORRECT = all(CI_LB < CI_UB, na.rm = TRUE)
  )

rm(list = ls())

###############
# two sample
###############
load_df <- function(fn) {
  if (str_detect(fn, "two")) {
    print(str_c("Possible Error. fn: ", fn))
  }
  fn <- str_c("results/", fn, collapse = "")
  DF <- readRDS(fn)
  DF <- DF %>%
    select(test, alt, stat, pvalue)
  return(DF)
}

fns <- c(
  "gaussian_type_one_one_way.rds",
  "gamma_type_one_rate_one_way.rds",
  "gamma_type_one_scale_one_way.rds",
  "gamma_type_one_shape_one_way.rds",
  "poisson_type_one_one_way.rds",
  "beta_type_one_one_way_shape1.rds",
  "beta_type_one_one_way_shape2.rds",
  "negative_binomial_type_one_one_way.rds",
  "exponential_type_one_one_way.rds",
  "binomail_type_one_one_way.rds",
  "cauchy_type_one_one_way.rds",
  "inverse_gaussian_type_one_one_way.rds",
  "empirical_mu_type_one_one_way.rds",
  "empirical_quantile_type_one_one_way.rds"
)

typeI <- map_dfr(fns, load_df)

typeI %>%
  drop_na() %>%
  nrow() == typeI %>%
  nrow()

typeI %>%
  distinct(test) %>%
  nrow() == 18

typeI %>%
  distinct(alt) %>%
  nrow() == 1

typeI %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeI %>%
  summarise(
    P_LB = all(pvalue >= 0),
    P_UB = all(pvalue <= 1)
  )

rm(list = ls())
