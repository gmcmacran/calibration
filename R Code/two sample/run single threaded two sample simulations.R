########################################
# Overview
#
# A script to run single threaded simulations
# one after another so that I don't have to
# monitor progress of each simulation.
########################################

library(stringr)
library(purrr)

# Get files to run
parent_dir <- "R Code/two sample"
r_files <- list.files(parent_dir)
for (dist in c("beta", "gamma", "inverse gaussian", "cauchy", "run")) {
  r_files <- r_files[!str_detect(r_files, dist)]
}
r_files

# Run simulations
run_sim <- function(r_file) {
  r_file <- str_c(parent_dir, r_file, sep = "/")
  msg <- str_c("Running: ", r_file)
  print(msg)
  source(r_file, local = new.env())
}

walk(r_files, run_sim)
