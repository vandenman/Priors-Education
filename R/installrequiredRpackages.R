# installs all R-packages required to reproduce the results

pkgs2used <- c(
  "coda",         # highest posterior density interval
  "rstan",        # general Bayesian inference
  "brms",         # fit Bayesian mixed effects models
  "bayesplot",    # visualize posteriors and convergence diagnostics
  "fitdistrplus", # fit distributions to posterior samples
  "readr",        # efficiently read .csv files
  "dplyr",        # manipulate raw data (e.g., select, na_if)
  "tidyr",        # manipulate raw data (e.g., gather)
  "ggplot2",      # general plotting
  "GGally",       # multiple panel descriptives plot of posterior
  "latex2exp"     # convert latex equations to R expressions for use in plots
)

pkgs2install <- setdiff(pkgs2used, installed.packages()[, "Package"])

install.packages(pkgs2install)
