library(tidyverse)

gamma <- bind_rows(
  readRDS("results/gamma_type_one_rate.rds"),
  readRDS("results/gamma_type_one_scale.rds"),
  readRDS("results/gamma_type_one_shape.rds")
)
gamma <- gamma %>%
  filter(test %in% c("gamma_rate_one_sample")) %>%
  select(test, alt, param = rate, pvalue) %>%
  bind_rows(
    gamma %>%
      filter(test %in% c("gamma_scale_one_sample")) %>%
      select(test, alt, param = scale, pvalue),
    gamma %>%
      filter(test %in% c("gamma_shape_one_sample")) %>%
      select(test, alt, param = shape, pvalue)
  )

invGauus <- bind_rows(
  readRDS("results/inverse_gaussian_type_one_mu.rds"),
  readRDS("results/inverse_gaussian_type_one_shape.rds"),
  readRDS("results/inverse_gaussian_type_one_dispersion.rds")
)

invGauus <- invGauus %>%
  filter(test == "inverse_gaussian_mu_one_sample") %>%
  select(test, alt, param = mu, pvalue) %>%
  bind_rows(
    invGauus %>%
      filter(test == "inverse_gaussian_shape_one_sample") %>%
      select(test, alt, param = shape, pvalue),
    invGauus %>%
      filter(test == "inverse_gaussian_dispersion_one_sample") %>%
      select(test, alt, param = dispersion, pvalue)
  ) %>%
  mutate(test = str_replace(test, "inverse_", "inv_")) %>%
  mutate(test = str_replace(test, "dispersion_", "disp_"))



typeI <- bind_rows(
  gamma,
  invGauus
)

temp <- typeI %>%
  group_by(test, param, alt) %>%
  summarize(all_pvalues = list(pvalue))

typeI <- typeI %>%
  inner_join(y = temp, by = c("test", "param", "alt"))

calc_emperical_pvalue <- function(individual_p_value, p_values) {
  p_values <- unlist(p_values)
  out <- mean(individual_p_value >= p_values)
  return(out)
}

typeI <- typeI %>%
  rowwise() %>%
  mutate(emperical_pvalue = calc_emperical_pvalue(pvalue, all_pvalues)) %>%
  ungroup()

plot_graph <- function(df, alt_arg) {
  drop_leading_zero <- function(l) {
    str_replace(l, "0(?=.)", "")
  }

  graph <- df %>%
    filter(alt == alt_arg) %>%
    mutate(test = str_replace(test, "_one_sample", "")) %>%
    ggplot(aes(x = pvalue, emperical_pvalue)) +
    geom_point(alpha = 0.3, shape = ".") +
    stat_function(fun = identity, color = "darkred") +
    scale_x_continuous(breaks = seq(0, 1, .2), labels = drop_leading_zero) +
    scale_y_continuous(breaks = seq(0, 1, .2), labels = drop_leading_zero) +
    labs(title = str_c("Alternative: ", alt_arg), x = "Asymptotic P Value", y = "Empirical P Value") +
    facet_wrap("~test")

  return(graph)
}

plot_graph(typeI, "greater")
plot_graph(typeI, "less")
plot_graph(typeI, "two.sided")
